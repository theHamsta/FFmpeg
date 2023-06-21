/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/opt.h"

#include "cbs.h"
#include "cbs_h265.h"
#include "h265_profile_level.h"
#include "h2645data.h"
#include "codec_internal.h"
#include "version.h"

#include "vulkan_encode.h"

enum UnitElems {
    UNIT_AUD        = 1 << 0,
};

typedef struct VulkanEncodeH265Context {
    FFVulkanEncodeContext vkenc;
    VkVideoEncodeH265ProfileInfoEXT profile;
    VkVideoEncodeH265CapabilitiesEXT caps;

    int bit_rate;
    int output_delay;
    int decode_delay;
    int gop_size;
    int b_per_p;
    int dpb_frames;
    int max_b_depth;
    int hrd_initial_buffer_fullness;
    int hrd_buffer_size;

    int surface_width;
    int surface_height;

    int min_cb, max_cb; //coding block is not CTB
    int min_ctb;
    int min_tbs, max_tbs;

    /* Options */
    enum UnitElems insert_units;
    int coder;
    int desired_b_depth;
    int tier;
    int level;

    /* State */
    enum UnitElems write_units;

    /* SPS structs */
    H265RawSPS raw_sps;
    StdVideoH265ScalingLists vksps_scaling;
    StdVideoH265HrdParameters vksps_vui_header;
    StdVideoH265SequenceParameterSetVui vksps_vui;
    StdVideoH265SequenceParameterSet vksps;

    /* PPS structs */
    H265RawPPS raw_pps;
    StdVideoH265ScalingLists vkpps_scaling;
    StdVideoH265PictureParameterSet vkpps;

    H265RawVPS raw_vps;
    StdVideoH265ProfileTierLevel vkvps_tier;
    StdVideoH265VideoParameterSet vkvps;
    /* Structs needed for CBC */
    H265RawAUD                  raw_aud;

    CodedBitstreamContext      *cbc;
    CodedBitstreamFragment      current_access_unit;
    int aud_needed;
    int sei_needed;
} VulkanEncodeH265Context;

typedef struct VulkanEncodeH265Picture {
    int64_t last_idr_frame;
    uint16_t idr_pic_id;
    uint16_t pic_order_cnt;
    StdVideoH265PictureType pic_type;
    StdVideoH265SliceType slice_type;

    StdVideoEncodeH265WeightTable slice_wt;
    StdVideoEncodeH265SliceSegmentHeader slice_segment_hdr;
    VkVideoEncodeH265NaluSliceSegmentInfoEXT vkslice;
    StdVideoEncodeH265PictureInfo h265pic_info;
    VkVideoEncodeH265PictureInfoEXT vkh265pic_info;
    VkVideoEncodeH265RateControlInfoEXT vkrc_info;
    VkVideoEncodeH265RateControlLayerInfoEXT vkrc_layer_info;

    VkVideoEncodeH265DpbSlotInfoEXT l0refs[37];
    VkVideoEncodeH265DpbSlotInfoEXT l1refs[37];
    StdVideoEncodeH265ReferenceInfo l0ref_info[37];
    StdVideoEncodeH265ReferenceInfo l1ref_info[37];

    StdVideoEncodeH265ReferenceListsInfo ref_list_info;

} VulkanEncodeH265Picture;

static av_cold int vulkan_encode_h265_init_sequence_params(AVCodecContext *avctx)
{
    VulkanEncodeH265Context             *enc = avctx->priv_data;

    H265RawSPS                          *sps = &enc->raw_sps;
    StdVideoH265ScalingLists            *vksps_scaling = &enc->vksps_scaling;
    StdVideoH265HrdParameters           *vksps_vui_header = &enc->vksps_vui_header;
    StdVideoH265SequenceParameterSetVui *vksps_vui = &enc->vksps_vui;
    StdVideoH265SequenceParameterSet    *vksps = &enc->vksps;
    H265RawVUI                          *vui = &sps->vui;

    H265RawPPS                          *pps = &enc->raw_pps;
    StdVideoH265ScalingLists            *vkpps_scaling = &enc->vkpps_scaling;
    StdVideoH265PictureParameterSet     *vkpps = &enc->vkpps;

    H265RawVPS                        *vps = &enc->raw_vps;
    H265RawProfileTierLevel           *ptl = &vps->profile_tier_level;
    StdVideoH265ProfileTierLevel      *vkvps_tier = &enc->vkvps_tier;
    StdVideoH265VideoParameterSet     *vkvps = &enc->vkvps;
    const AVPixFmtDescriptor *desc;
    int chroma_format, bit_depth;
    int i;

    desc = av_pix_fmt_desc_get(avctx->sw_pix_fmt);
    av_assert0(desc);
    if (desc->nb_components == 1) {
        chroma_format = 0;
    } else {
        if (desc->log2_chroma_w == 1 && desc->log2_chroma_h == 1) {
            chroma_format = 1;
        } else if (desc->log2_chroma_w == 1 && desc->log2_chroma_h == 0) {
            chroma_format = 2;
        } else if (desc->log2_chroma_w == 0 && desc->log2_chroma_h == 0) {
            chroma_format = 3;
        } else {
            av_log(avctx, AV_LOG_ERROR, "Chroma format of input pixel format "
                   "%s is not supported.\n", desc->name);
            return AVERROR(EINVAL);
        }
    }
    bit_depth = desc->comp[0].depth;

    memset(vps, 0, sizeof(*vps));
    memset(sps, 0, sizeof(*sps));
    memset(pps, 0, sizeof(*pps));

    vps->nal_unit_header = (H265RawNALUnitHeader) {
        .nal_unit_type         = HEVC_NAL_VPS,
        .nuh_layer_id          = 0,
        .nuh_temporal_id_plus1 = 1,
    };

    vps->vps_video_parameter_set_id = 0;

    vps->vps_base_layer_internal_flag  = 1;
    vps->vps_base_layer_available_flag = 1;
    vps->vps_max_layers_minus1         = 0;
    vps->vps_max_sub_layers_minus1     = 0;
    vps->vps_temporal_id_nesting_flag  = 1;

    ptl->general_profile_space = 0;
    ptl->general_profile_idc   = avctx->profile;
    ptl->general_tier_flag     = enc->tier;

    ptl->general_profile_compatibility_flag[ptl->general_profile_idc] = 1;

    if (ptl->general_profile_compatibility_flag[1])
        ptl->general_profile_compatibility_flag[2] = 1;
    if (ptl->general_profile_compatibility_flag[3]) {
        ptl->general_profile_compatibility_flag[1] = 1;
        ptl->general_profile_compatibility_flag[2] = 1;
    }

    ptl->general_progressive_source_flag    = 1;
    ptl->general_interlaced_source_flag     = 0;
    ptl->general_non_packed_constraint_flag = 1;
    ptl->general_frame_only_constraint_flag = 1;

    ptl->general_max_14bit_constraint_flag = bit_depth <= 14;
    ptl->general_max_12bit_constraint_flag = bit_depth <= 12;
    ptl->general_max_10bit_constraint_flag = bit_depth <= 10;
    ptl->general_max_8bit_constraint_flag  = bit_depth ==  8;

    ptl->general_max_422chroma_constraint_flag  = chroma_format <= 2;
    ptl->general_max_420chroma_constraint_flag  = chroma_format <= 1;
    ptl->general_max_monochrome_constraint_flag = chroma_format == 0;

    //    ptl->general_intra_constraint_flag = ctx->gop_size == 1;
    ptl->general_one_picture_only_constraint_flag = 0;

    ptl->general_lower_bit_rate_constraint_flag = 1;

    if (avctx->level != FF_LEVEL_UNKNOWN) {
        ptl->general_level_idc = avctx->level;
    } else {
        const H265LevelDescriptor *level;

        level = ff_h265_guess_level(ptl, avctx->bit_rate,
                                    enc->surface_width, enc->surface_height,
                                    1, 0, 0, 1);
        if (level) {
            av_log(avctx, AV_LOG_VERBOSE, "Using level %s.\n", level->name);
            ptl->general_level_idc = level->level_idc;
        } else {
            av_log(avctx, AV_LOG_VERBOSE, "Stream will not conform to "
                   "any normal level; using level 8.5.\n");
            ptl->general_level_idc = 255;
            // The tier flag must be set in level 8.5.
            ptl->general_tier_flag = 1;
        }
    }

    vps->vps_sub_layer_ordering_info_present_flag = 0;
    vps->vps_max_dec_pic_buffering_minus1[0]      = enc->max_b_depth + 1;
    vps->vps_max_num_reorder_pics[0]              = enc->max_b_depth;
    vps->vps_max_latency_increase_plus1[0]        = 0;

    vps->vps_max_layer_id             = 0;
    vps->vps_num_layer_sets_minus1    = 0;
    vps->layer_id_included_flag[0][0] = 1;

    vps->vps_timing_info_present_flag = 1;
    if (avctx->framerate.num > 0 && avctx->framerate.den > 0) {
        vps->vps_num_units_in_tick  = avctx->framerate.den;
        vps->vps_time_scale         = avctx->framerate.num;
        vps->vps_poc_proportional_to_timing_flag = 1;
        vps->vps_num_ticks_poc_diff_one_minus1   = 0;
    } else {
        vps->vps_num_units_in_tick  = avctx->time_base.num;
        vps->vps_time_scale         = avctx->time_base.den;
        vps->vps_poc_proportional_to_timing_flag = 0;
    }
    vps->vps_num_hrd_parameters = 0;

    sps->nal_unit_header = (H265RawNALUnitHeader) {
        .nal_unit_type         = HEVC_NAL_SPS,
        .nuh_layer_id          = 0,
        .nuh_temporal_id_plus1 = 1,
    };

    sps->sps_video_parameter_set_id = vps->vps_video_parameter_set_id;

    sps->sps_max_sub_layers_minus1    = vps->vps_max_sub_layers_minus1;
    sps->sps_temporal_id_nesting_flag = vps->vps_temporal_id_nesting_flag;

    sps->profile_tier_level = vps->profile_tier_level;

    sps->sps_seq_parameter_set_id = 0;

    sps->chroma_format_idc          = chroma_format;
    sps->separate_colour_plane_flag = 0;

    sps->pic_width_in_luma_samples  = enc->surface_width;
    sps->pic_height_in_luma_samples = enc->surface_height;
#if 0
    if (avctx->width  != enc->surface_width ||
        avctx->height != enc->surface_height) {
        sps->conformance_window_flag = 1;
        sps->conf_win_left_offset   = 0;
        sps->conf_win_right_offset  =
            (enc->surface_width - avctx->width) >> desc->log2_chroma_w;
        sps->conf_win_top_offset    = 0;
        sps->conf_win_bottom_offset =
            (enc->surface_height - avctx->height) >> desc->log2_chroma_h;
    } else {
        sps->conformance_window_flag = 0;
    }
#endif
    sps->bit_depth_luma_minus8   = bit_depth - 8;
    sps->bit_depth_chroma_minus8 = bit_depth - 8;

    sps->log2_max_pic_order_cnt_lsb_minus4 = 4;

    sps->sps_sub_layer_ordering_info_present_flag =
        vps->vps_sub_layer_ordering_info_present_flag;
    for (i = 0; i <= sps->sps_max_sub_layers_minus1; i++) {
        sps->sps_max_dec_pic_buffering_minus1[i] =
            vps->vps_max_dec_pic_buffering_minus1[i];
        sps->sps_max_num_reorder_pics[i] =
            vps->vps_max_num_reorder_pics[i];
        sps->sps_max_latency_increase_plus1[i] =
            vps->vps_max_latency_increase_plus1[i];
    }

    // These values come from the capabilities of the first encoder
    // implementation in the i965 driver on Intel Skylake.  They may
    // fail badly with other platforms or drivers.
    // CTB size from 16x16 to 64x64.
    // CB size minimum can be 8x8.
    sps->log2_min_luma_coding_block_size_minus3   = av_log2(enc->min_cb) - 3;
    sps->log2_diff_max_min_luma_coding_block_size = av_log2(enc->max_cb) - av_log2(enc->min_cb);
    // Transform size from 4x4 to 32x32.
    sps->log2_min_luma_transform_block_size_minus2   = av_log2(enc->min_tbs) - 2;
    sps->log2_diff_max_min_luma_transform_block_size = av_log2(enc->max_tbs) - av_log2(enc->min_tbs);
    // Full transform hierarchy allowed (2-5).
    sps->max_transform_hierarchy_depth_inter = 4;
    sps->max_transform_hierarchy_depth_intra = 4;
    // AMP works.
    sps->amp_enabled_flag = 1;
    sps->sample_adaptive_offset_enabled_flag = 0;
    if (enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SAMPLE_ADAPTIVE_OFFSET_ENABLED_FLAG_SET_BIT_EXT)
        sps->sample_adaptive_offset_enabled_flag = 1;
    sps->sps_temporal_mvp_enabled_flag       = 0;
    sps->pcm_enabled_flag = 0;

    sps->vui_parameters_present_flag = 0;

    if (avctx->sample_aspect_ratio.num != 0 &&
        avctx->sample_aspect_ratio.den != 0) {
        int num, den, i;
        av_reduce(&num, &den, avctx->sample_aspect_ratio.num,
                  avctx->sample_aspect_ratio.den, 65535);
        for (i = 0; i < FF_ARRAY_ELEMS(ff_h2645_pixel_aspect); i++) {
            if (num == ff_h2645_pixel_aspect[i].num &&
                den == ff_h2645_pixel_aspect[i].den) {
                vui->aspect_ratio_idc = i;
                break;
            }
        }
        if (i >= FF_ARRAY_ELEMS(ff_h2645_pixel_aspect)) {
            vui->aspect_ratio_idc = 255;
            vui->sar_width  = num;
            vui->sar_height = den;
        }
        vui->aspect_ratio_info_present_flag = 1;
    }
    vui->aspect_ratio_idc = 0;

    /* Unspecified video format, from table E-2. */
    vui->video_format             = 5;
    vui->video_full_range_flag    = avctx->color_range == AVCOL_RANGE_JPEG;
    vui->colour_primaries         = avctx->color_primaries;
    vui->transfer_characteristics = avctx->color_trc;
    vui->matrix_coefficients      = avctx->colorspace;
    if (avctx->color_primaries != AVCOL_PRI_UNSPECIFIED ||
        avctx->color_trc       != AVCOL_TRC_UNSPECIFIED ||
        avctx->colorspace      != AVCOL_SPC_UNSPECIFIED)
        vui->colour_description_present_flag = 1;
    if (avctx->color_range     != AVCOL_RANGE_UNSPECIFIED ||
        vui->colour_description_present_flag)
        vui->video_signal_type_present_flag = 1;

    if (avctx->chroma_sample_location != AVCHROMA_LOC_UNSPECIFIED) {
        vui->chroma_loc_info_present_flag = 1;
        vui->chroma_sample_loc_type_top_field =
            vui->chroma_sample_loc_type_bottom_field =
                avctx->chroma_sample_location - 1;
    }

    vui->vui_timing_info_present_flag = 1;
    vui->vui_num_units_in_tick               = vps->vps_num_units_in_tick;
    vui->vui_time_scale                      = vps->vps_time_scale;
    vui->vui_poc_proportional_to_timing_flag = vps->vps_poc_proportional_to_timing_flag;
    vui->vui_num_ticks_poc_diff_one_minus1   = vps->vps_num_ticks_poc_diff_one_minus1;
    vui->vui_hrd_parameters_present_flag     = 0;

    vui->bitstream_restriction_flag    = 1;
    vui->motion_vectors_over_pic_boundaries_flag = 1;
    vui->restricted_ref_pic_lists_flag = 1;
    vui->max_bytes_per_pic_denom       = 2;
    vui->max_bits_per_min_cu_denom     = 1;
    vui->log2_max_mv_length_horizontal = 15;
    vui->log2_max_mv_length_vertical   = 15;

    // PPS

    pps->nal_unit_header = (H265RawNALUnitHeader) {
        .nal_unit_type         = HEVC_NAL_PPS,
        .nuh_layer_id          = 0,
        .nuh_temporal_id_plus1 = 1,
    };

    pps->pps_pic_parameter_set_id = 0;
    pps->pps_seq_parameter_set_id = sps->sps_seq_parameter_set_id;

    pps->num_ref_idx_l0_default_active_minus1 = 0;
    pps->num_ref_idx_l1_default_active_minus1 = 0;

    pps->cabac_init_present_flag = 1;
    pps->dependent_slice_segments_enabled_flag = 1;
    pps->init_qp_minus26 = 0; // TODO - fix, I have no idea
    pps->cu_qp_delta_enabled_flag = 0;//(ctx->va_rc_mode != VA_RC_CQP);
    pps->diff_cu_qp_delta_depth   = 0;

    if (enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_TRANSFORM_SKIP_ENABLED_FLAG_SET_BIT_EXT)
        pps->transform_skip_enabled_flag = 1;

    pps->pps_loop_filter_across_slices_enabled_flag = 0;
    pps->deblocking_filter_control_present_flag = 1;
    if (enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_CONSTRAINED_INTRA_PRED_FLAG_SET_BIT_EXT)
        pps->constrained_intra_pred_flag = 1;
    //    pps->diff_cu_qp_delta_depth = sps->log2_diff_max_min_luma_coding_block_size;

    *vksps_scaling = (StdVideoH265ScalingLists) {
    };

    *vksps_vui_header = (StdVideoH265HrdParameters) {
    };

    *vksps_vui = (StdVideoH265SequenceParameterSetVui) {
        .aspect_ratio_idc = vui->aspect_ratio_idc,
        .sar_width = vui->sar_width,
        .sar_height = vui->sar_height,
        .video_format = vui->video_format,
        .colour_primaries = vui->colour_primaries,
        .transfer_characteristics = vui->transfer_characteristics,
        .vui_num_units_in_tick = vui->vui_num_units_in_tick,
        .vui_time_scale = vui->vui_time_scale,
        .pHrdParameters = vksps_vui_header,
        .flags = (StdVideoH265SpsVuiFlags) {
            .aspect_ratio_info_present_flag = vui->aspect_ratio_info_present_flag,
            .overscan_info_present_flag = vui->overscan_info_present_flag,
            .overscan_appropriate_flag = vui->overscan_appropriate_flag,
            .video_signal_type_present_flag = vui->video_signal_type_present_flag,
            .video_full_range_flag = vui->video_full_range_flag,
            .chroma_loc_info_present_flag = vui->chroma_loc_info_present_flag,
            .vui_timing_info_present_flag = vui->vui_timing_info_present_flag,
            .bitstream_restriction_flag = vui->bitstream_restriction_flag,
            .vui_hrd_parameters_present_flag = sps->vui_parameters_present_flag,
        },
    };

    *vksps = (StdVideoH265SequenceParameterSet) {
        .sps_video_parameter_set_id = sps->sps_video_parameter_set_id,
        .chroma_format_idc = sps->chroma_format_idc,
        .bit_depth_luma_minus8 = sps->bit_depth_luma_minus8,
        .bit_depth_chroma_minus8 = sps->bit_depth_chroma_minus8,
        .pic_width_in_luma_samples  = sps->pic_width_in_luma_samples,
        .pic_height_in_luma_samples = sps->pic_height_in_luma_samples,
        .log2_min_luma_coding_block_size_minus3 = sps->log2_min_luma_coding_block_size_minus3,
        .flags = (StdVideoH265SpsFlags) {
            .strong_intra_smoothing_enabled_flag = sps->strong_intra_smoothing_enabled_flag,
            .amp_enabled_flag = sps->amp_enabled_flag,
            .sample_adaptive_offset_enabled_flag = sps->sample_adaptive_offset_enabled_flag,
        },
        .pSequenceParameterSetVui = vksps_vui,
    };

    *vkpps_scaling = (StdVideoH265ScalingLists) {
    };

    *vkpps = (StdVideoH265PictureParameterSet) {
        .pps_seq_parameter_set_id = pps->pps_seq_parameter_set_id,
        .pps_pic_parameter_set_id = pps->pps_pic_parameter_set_id,
        .num_ref_idx_l0_default_active_minus1 = pps->num_ref_idx_l0_default_active_minus1,
        .num_ref_idx_l1_default_active_minus1 = pps->num_ref_idx_l1_default_active_minus1,
        .init_qp_minus26 = pps->init_qp_minus26,
        .flags = (StdVideoH265PpsFlags) {
            .constrained_intra_pred_flag = pps->constrained_intra_pred_flag,
            .cabac_init_present_flag = pps->cabac_init_present_flag,
            .transform_skip_enabled_flag = pps->transform_skip_enabled_flag,
            .cu_qp_delta_enabled_flag = pps->cu_qp_delta_enabled_flag,
            .deblocking_filter_control_present_flag = pps->deblocking_filter_control_present_flag,
        },
    };

    *vkvps_tier = (StdVideoH265ProfileTierLevel) {
        .general_profile_idc = ptl->general_profile_idc,
        .general_level_idc = ptl->general_level_idc,
        .flags = (StdVideoH265ProfileTierLevelFlags) {
            .general_tier_flag = ptl->general_tier_flag,
            .general_progressive_source_flag = ptl->general_progressive_source_flag,
            .general_interlaced_source_flag = ptl->general_interlaced_source_flag,
            .general_non_packed_constraint_flag = ptl->general_non_packed_constraint_flag,
            .general_frame_only_constraint_flag = ptl->general_frame_only_constraint_flag,
        },
    };
    *vkvps = (StdVideoH265VideoParameterSet) {
        .vps_video_parameter_set_id = vps->vps_video_parameter_set_id,
        .vps_max_sub_layers_minus1 = vps->vps_max_sub_layers_minus1,
        .vps_num_units_in_tick = vps->vps_num_units_in_tick,
        .vps_time_scale = vps->vps_time_scale,
        .vps_num_ticks_poc_diff_one_minus1 = vps->vps_num_ticks_poc_diff_one_minus1,
        .flags = (StdVideoH265VpsFlags) {
            .vps_temporal_id_nesting_flag = vps->vps_temporal_id_nesting_flag,
            .vps_sub_layer_ordering_info_present_flag = vps->vps_sub_layer_ordering_info_present_flag,
            .vps_timing_info_present_flag = vps->vps_timing_info_present_flag,
            .vps_poc_proportional_to_timing_flag = vps->vps_poc_proportional_to_timing_flag,
        },
        .pProfileTierLevel = vkvps_tier,
    };

    return 0;
}

static int vulkan_encode_h265_add_nal(AVCodecContext *avctx,
                                      CodedBitstreamFragment *au,
                                      void *nal_unit)
{
    H265RawNALUnitHeader *header = nal_unit;

    int err = ff_cbs_insert_unit_content(au, -1,
                                         header->nal_unit_type, nal_unit, NULL);
    if (err < 0)
        av_log(avctx, AV_LOG_ERROR, "Failed to add NAL unit: "
               "type = %d.\n", header->nal_unit_type);

    return err;
}

static int vulkan_encode_h265_write_access_unit(AVCodecContext *avctx,
                                                uint8_t *data, size_t *data_len,
                                                CodedBitstreamFragment *au)
{
    VulkanEncodeH265Context *enc = avctx->priv_data;

    int err = ff_cbs_write_fragment_data(enc->cbc, au);
    if (err < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed to write packed header.\n");
        return err;
    }

    if (*data_len < au->data_size) {
        av_log(avctx, AV_LOG_ERROR, "Access unit too large: "
               "%zu < %zu.\n", *data_len,
               au->data_size);
        return AVERROR(ENOSPC);
    }

    memcpy(data, au->data, au->data_size);
    *data_len = au->data_size;

    return 0;
}

static int vulkan_encode_h265_write_sequence_header(AVCodecContext *avctx,
                                                    uint8_t *data, size_t *data_len)
{
    int err;
    VulkanEncodeH265Context *enc = avctx->priv_data;
    CodedBitstreamFragment   *au = &enc->current_access_unit;

    if (enc->write_units & UNIT_AUD) {
        err = vulkan_encode_h265_add_nal(avctx, au, &enc->raw_aud);
        if (err < 0)
            goto fail;
    }

    err = vulkan_encode_h265_add_nal(avctx, au, &enc->raw_vps);
    if (err < 0)
        goto fail;

    err = vulkan_encode_h265_add_nal(avctx, au, &enc->raw_sps);
    if (err < 0)
        goto fail;

    err = vulkan_encode_h265_add_nal(avctx, au, &enc->raw_pps);
    if (err < 0)
        goto fail;

    err = vulkan_encode_h265_write_access_unit(avctx, data, data_len, au);
fail:
    ff_cbs_fragment_reset(au);
    return err;
}

static av_cold int vulkan_encode_h265_create_session(AVCodecContext *avctx)
{
    VkResult ret;
    VulkanEncodeH265Context *enc = avctx->priv_data;
    FFVulkanFunctions *vk = &enc->vkenc.s.vkfn;

    VkVideoEncodeH265SessionParametersAddInfoEXT h265_params_info;
    VkVideoEncodeH265SessionParametersCreateInfoEXT h265_params;
    VkVideoSessionParametersCreateInfoKHR session_params_create;

    h265_params_info = (VkVideoEncodeH265SessionParametersAddInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_ADD_INFO_EXT,
        .pStdSPSs = &enc->vksps,
        .stdSPSCount = 1,
        .pStdPPSs = &enc->vkpps,
        .stdPPSCount = 1,
        .pStdVPSs = &enc->vkvps,
        .stdVPSCount = 1,
    };
    h265_params = (VkVideoEncodeH265SessionParametersCreateInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_SESSION_PARAMETERS_CREATE_INFO_EXT,
        .maxStdSPSCount = 1,
        .maxStdPPSCount = 1,
        .maxStdVPSCount = 1,
        .pParametersAddInfo = &h265_params_info,
    };
    session_params_create = (VkVideoSessionParametersCreateInfoKHR) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_SESSION_PARAMETERS_CREATE_INFO_KHR,
        .pNext = &h265_params,
        .videoSession = enc->vkenc.common.session,
        .videoSessionParametersTemplate = NULL,
    };

    /* Create session parameters */
    ret = vk->CreateVideoSessionParametersKHR(enc->vkenc.s.hwctx->act_dev, &session_params_create,
                                              enc->vkenc.s.hwctx->alloc, &enc->vkenc.session_params);
    if (ret != VK_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Unable to create Vulkan video session parameters: %s!\n",
               ff_vk_ret2str(ret));
        return AVERROR_EXTERNAL;
    }

    return 0;
}

static int vulkan_encode_h265_init_pic_headers(AVCodecContext *avctx,
                                               FFVulkanEncodePicture *pic)
{
    VulkanEncodeH265Context    *enc = avctx->priv_data;
    VulkanEncodeH265Picture   *hpic = pic->priv_data;
    FFVulkanEncodePicture     *prev = pic->prev;
    VulkanEncodeH265Picture  *hprev = prev ? prev->priv_data : NULL;

    int qp = pic->qp;

    if (pic->type == FF_VK_FRAME_KEY) {
        av_assert0(pic->display_order == pic->encode_order);

        hpic->last_idr_frame = pic->display_order;

        hpic->slice_type     = STD_VIDEO_H265_SLICE_TYPE_I;
        hpic->pic_type       = STD_VIDEO_H265_PICTURE_TYPE_IDR;
    } else {
        av_assert0(prev);

        hpic->last_idr_frame = hprev->last_idr_frame;

        if (pic->type == FF_VK_FRAME_I) {
            hpic->slice_type     = STD_VIDEO_H265_SLICE_TYPE_I;
            hpic->pic_type       = STD_VIDEO_H265_PICTURE_TYPE_I;
        } else if (pic->type == FF_VK_FRAME_P) {
            av_assert0(pic->refs[0]);
            hpic->slice_type     = STD_VIDEO_H265_SLICE_TYPE_P;
            hpic->pic_type       = STD_VIDEO_H264_PICTURE_TYPE_P;
        } else {
            hpic->slice_type = STD_VIDEO_H264_SLICE_TYPE_B;
            hpic->pic_type   = STD_VIDEO_H264_PICTURE_TYPE_B;
        }
    }

    hpic->pic_order_cnt = pic->display_order - hpic->last_idr_frame;

    enc->write_units = 0x0;

    if (enc->insert_units & UNIT_AUD) {
        enc->raw_aud = (H265RawAUD) {
            .nal_unit_header = {
                .nal_unit_type = HEVC_NAL_AUD,
                .nuh_temporal_id_plus1 = 1,
            },
            .pic_type = 0,
        };
        enc->write_units |= UNIT_AUD;
    }

    hpic->slice_wt = (StdVideoEncodeH265WeightTable) {
        .flags = (StdVideoEncodeH265WeightTableFlags) {
            .luma_weight_l0_flag = 0,
            .chroma_weight_l0_flag = 0,
            .luma_weight_l1_flag = 0,
            .chroma_weight_l1_flag = 0,
        },
        .luma_log2_weight_denom = 0,
        .delta_chroma_log2_weight_denom = 0,
        .delta_luma_weight_l0 = { 0 },
        .luma_offset_l0 = { 0 },
        .delta_chroma_weight_l0 = { { 0 } },
        .delta_chroma_offset_l0 = { { 0 } },
        .delta_luma_weight_l1 = { 0 },
        .luma_offset_l1 = { 0 },
        .delta_chroma_weight_l1 = { { 0 } },
        .delta_chroma_offset_l1 = { { 0 } },
    };

    hpic->slice_segment_hdr = (StdVideoEncodeH265SliceSegmentHeader) {
        .flags = (StdVideoEncodeH265SliceSegmentHeaderFlags) {
            .first_slice_segment_in_pic_flag = 1,
            .dependent_slice_segment_flag = 0,
            .num_ref_idx_active_override_flag = 0,
            .slice_loop_filter_across_slices_enabled_flag = 0,//pps->pps_loop_filter_across_slices_enabled_flag,
            .slice_deblocking_filter_disabled_flag = 0,
            .cabac_init_flag = 0,
        },
        .slice_type = hpic->slice_type,
        .MaxNumMergeCand = 0,
        .pWeightTable = &hpic->slice_wt,
    };

    hpic->vkslice = (VkVideoEncodeH265NaluSliceSegmentInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_NALU_SLICE_SEGMENT_INFO_EXT,
        .pNext = NULL,
        .pStdSliceSegmentHeader = &hpic->slice_segment_hdr,
    };

    hpic->h265pic_info = (StdVideoEncodeH265PictureInfo) {
        .flags = (StdVideoEncodeH265PictureInfoFlags) {
            .is_reference = pic->is_reference,
            //            .IrapPicFlag = pic->type == FF_VK_FRAME_KEY,
            .used_for_long_term_reference = 0,
            .discardable_flag = 0,
            .cross_layer_bla_flag = 0,
            .pic_output_flag = 0,
            .no_output_of_prior_pics_flag = 0,
            .short_term_ref_pic_set_sps_flag = 0,
            .slice_temporal_mvp_enabled_flag = 0,
            /* Reserved */
        },
        .sps_video_parameter_set_id = enc->raw_sps.sps_video_parameter_set_id,
        .pps_seq_parameter_set_id = enc->raw_sps.sps_seq_parameter_set_id,
        .pps_pic_parameter_set_id = enc->raw_pps.pps_pic_parameter_set_id,
        .pic_type = hpic->pic_type,
        .PicOrderCntVal = hpic->pic_order_cnt,
        .TemporalId = 0, /* ? */
        .pRefLists = &hpic->ref_list_info,
    };

    hpic->ref_list_info = (StdVideoEncodeH265ReferenceListsInfo) {
        .flags                    = (StdVideoEncodeH265ReferenceListsInfoFlags) {
            .ref_pic_list_modification_flag_l0 = 0,
            .ref_pic_list_modification_flag_l1 = 0,
        },
    };

    for (int i = 0; i < pic->nb_refs; i++) {
        FFVulkanEncodePicture *ref = pic->refs[i];
        VulkanEncodeH265Picture *href = ref->priv_data;

        hpic->l0ref_info[0] = (StdVideoEncodeH265ReferenceInfo) {
            .flags = (StdVideoEncodeH265ReferenceInfoFlags) {
                .used_for_long_term_reference = 0,
                .unused_for_reference = 0,
            },
            .PicOrderCntVal = href->pic_order_cnt,
            .pic_type = 0,
            .TemporalId = 0,
        };

        hpic->l0refs[i] = (VkVideoEncodeH265DpbSlotInfoEXT) {
            .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_DPB_SLOT_INFO_EXT,
            .pStdReferenceInfo = &hpic->l0ref_info[i],
        };
    }

    hpic->vkh265pic_info = (VkVideoEncodeH265PictureInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PICTURE_INFO_EXT,
        .pNext = NULL,
        .naluSliceSegmentEntryCount = 1,
        .pNaluSliceSegmentEntries = &hpic->vkslice,
        .pStdPictureInfo = &hpic->h265pic_info,
    };

    hpic->vkrc_info = (VkVideoEncodeH265RateControlInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_INFO_EXT,
    };

    hpic->vkrc_layer_info = (VkVideoEncodeH265RateControlLayerInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_RATE_CONTROL_LAYER_INFO_EXT,
        .pNext = NULL,
        .minQp = (VkVideoEncodeH265QpEXT){ qp, qp, qp },
        .maxQp = (VkVideoEncodeH265QpEXT){ qp, qp, qp },
        .useMinQp = 1,
        .useMaxQp = 1,
    };

    pic->codec_info     = &hpic->vkh265pic_info;
    pic->codec_layer    = &hpic->vkrc_info;
    pic->codec_rc_layer = &hpic->vkrc_layer_info;

    return 0;
}

static int vulkan_encode_h265_write_extra_headers(AVCodecContext *avctx,
                                                  FFVulkanEncodePicture *pic,
                                                  uint8_t *data, size_t *data_len)
{
    int err;
    VulkanEncodeH265Context *enc = avctx->priv_data;
    CodedBitstreamFragment   *au = &enc->current_access_unit;

    if (enc->write_units) {
        if (enc->write_units & UNIT_AUD) {
            err = vulkan_encode_h265_add_nal(avctx, au, &enc->raw_aud);
            if (err < 0)
                goto fail;
        }

        err = vulkan_encode_h265_write_access_unit(avctx, data, data_len, au);
        if (err < 0)
            goto fail;

        ff_cbs_fragment_reset(au);

        return 0;
    }

fail:
    ff_cbs_fragment_reset(au);
    return err;
}

static int vulkan_encode_h265_write_filler(AVCodecContext *avctx, uint32_t filler,
                                           uint8_t *data, size_t *data_len)
{
    int err = 0;
    VulkanEncodeH265Context *enc = avctx->priv_data;
    CodedBitstreamFragment   *au = &enc->current_access_unit;

    H265RawFiller raw_filler = {
        .nal_unit_header = {
            .nal_unit_type = HEVC_NAL_FD_NUT,
            .nuh_temporal_id_plus1 = 1,
        },
        .filler_size = filler,
    };

    err = vulkan_encode_h265_add_nal(avctx, au, &raw_filler);
    if (err < 0)
        goto fail;

    err = vulkan_encode_h265_write_access_unit(avctx, data, data_len, au);
fail:
    ff_cbs_fragment_reset(au);
    return err;
}

static const FFVulkanEncoder encoder = {
    .pic_priv_data_size = sizeof(VulkanEncodeH265Picture),
    .write_stream_headers = vulkan_encode_h265_write_sequence_header,
    .init_pic_headers = vulkan_encode_h265_init_pic_headers,
    .write_filler = vulkan_encode_h265_write_filler,
    .write_extra_headers = vulkan_encode_h265_write_extra_headers,
};

static av_cold int vulkan_encode_h265_init(AVCodecContext *avctx)
{
    int err;
    VulkanEncodeH265Context *enc = avctx->priv_data;

    if (avctx->profile == FF_PROFILE_UNKNOWN)
        avctx->profile = enc->vkenc.opts.profile;
    if (avctx->level == FF_LEVEL_UNKNOWN)
        avctx->level = enc->level;

    enc->profile = (VkVideoEncodeH265ProfileInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_PROFILE_INFO_EXT,
        .stdProfileIdc = enc->vkenc.opts.profile,
    };

    enc->caps = (VkVideoEncodeH265CapabilitiesEXT) {
        .sType = VK_STRUCTURE_TYPE_VIDEO_ENCODE_H265_CAPABILITIES_EXT,
    };

    err = ff_cbs_init(&enc->cbc, AV_CODEC_ID_H265, avctx);
    if (err < 0)
        return err;

    enc->bit_rate    = avctx->bit_rate;
    enc->gop_size    = 3; /* avctx->gop_size; */
    enc->b_per_p     = 0; /* avctx->max_b_frames; */
    enc->max_b_depth = 0; /* FFMIN(enc->desired_b_depth,
                             av_log2(enc->b_per_p) + 1); */

    enc->vkenc.gop_size = enc->gop_size;
    enc->vkenc.bitrate =enc->bit_rate;

    err = ff_vulkan_encode_init(avctx, &enc->vkenc, &enc->profile, &enc->caps,
                                &encoder, enc->b_per_p, enc->max_b_depth);
    if (err < 0)
        return err;

    av_log(avctx, AV_LOG_VERBOSE, "H265 encoder capabilities:\n");
    av_log(avctx, AV_LOG_VERBOSE, "    Standard capability flags:\n");
    av_log(avctx, AV_LOG_VERBOSE, "        separate_color_plane: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SEPARATE_COLOR_PLANE_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        sample_adaptive: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SAMPLE_ADAPTIVE_OFFSET_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        scaling_lists: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SCALING_LIST_DATA_PRESENT_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        pcm_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_PCM_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        sps_temporal_mvp_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SPS_TEMPORAL_MVP_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        init_qp_minus26: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_INIT_QP_MINUS26_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        weighted:%s%s\n",
           enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_WEIGHTED_PRED_FLAG_SET_BIT_EXT ?
               " pred" : "",
           enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_WEIGHTED_BIPRED_FLAG_SET_BIT_EXT ?
	   " bipred" : "");
    av_log(avctx, AV_LOG_VERBOSE, "        log2_parallel_merge_level_minus2: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_LOG2_PARALLEL_MERGE_LEVEL_MINUS2_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        sign_data_hiding_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_SIGN_DATA_HIDING_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        transform_skip_enabled_set: %d\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_TRANSFORM_SKIP_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        transform_skip_enabled_unset: %d\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_TRANSFORM_SKIP_ENABLED_FLAG_UNSET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        pps_slice_chroma_qp_offsets_present: %d\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_PPS_SLICE_CHROMA_QP_OFFSETS_PRESENT_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        transquant_bypass_enabled: %d\n",
           !!(enc->caps.stdSyntaxFlags &  VK_VIDEO_ENCODE_H265_STD_TRANSQUANT_BYPASS_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        constrained_intra_pred: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_CONSTRAINED_INTRA_PRED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        entropy_coding_sync_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_ENTROPY_CODING_SYNC_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        deblocking_filter_override_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_DEBLOCKING_FILTER_OVERRIDE_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        dependent_slice_segments_enabled: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_DEPENDENT_SLICE_SEGMENTS_ENABLED_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        dependent_slice_segment: %i\n",
           !!(enc->caps.stdSyntaxFlags & VK_VIDEO_ENCODE_H265_STD_DEPENDENT_SLICE_SEGMENT_FLAG_SET_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "    Capability flags:\n");
    av_log(avctx, AV_LOG_VERBOSE, "        hrd_compliance: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_HRD_COMPLIANCE_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        pred_weight_table_generated: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_PREDICTION_WEIGHT_TABLE_GENERATED_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        row_unaligned_slice_segment: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_ROW_UNALIGNED_SLICE_SEGMENT_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        different_slice_segment_type: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_DIFFERENT_SLICE_SEGMENT_TYPE_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        b_frame_in_l0_list: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_B_FRAME_IN_L0_LIST_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        b_frame_in_l1_list: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_B_FRAME_IN_L1_LIST_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        per_pict_type_min_max_qp: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_PER_PICTURE_TYPE_MIN_MAX_QP_BIT_EXT));
    av_log(avctx, AV_LOG_VERBOSE, "        per_slice_segment_constant_qp: %i\n",
           !!(enc->caps.flags & VK_VIDEO_ENCODE_H265_CAPABILITY_PER_SLICE_SEGMENT_CONSTANT_QP_BIT_EXT));

    av_log(avctx, AV_LOG_VERBOSE, "    Capabilities:\n");
    av_log(avctx, AV_LOG_VERBOSE, "        maxLevelIdc: %i\n",
           enc->caps.maxLevelIdc);
    av_log(avctx, AV_LOG_VERBOSE, "        maxSliceSegmentCount: %i\n",
           enc->caps.maxSliceSegmentCount);
    av_log(avctx, AV_LOG_VERBOSE, "    max(P/B)PictureL0ReferenceCount: %i P's; %i B's\n",
           enc->caps.maxPPictureL0ReferenceCount,
           enc->caps.maxBPictureL0ReferenceCount);
    av_log(avctx, AV_LOG_VERBOSE, "    maxL1ReferenceCount: %i\n",
           enc->caps.maxL1ReferenceCount);
    av_log(avctx, AV_LOG_VERBOSE, "    expectDyadicTemporalSubLayerPattern: %i\n",
           enc->caps.expectDyadicTemporalSubLayerPattern);
    av_log(avctx, AV_LOG_VERBOSE, "    min/max Qp: [%i, %i]\n",
           enc->caps.minQp, enc->caps.maxQp);
    av_log(avctx, AV_LOG_VERBOSE, "    prefersGopRemainingFrames: %i\n",
           enc->caps.prefersGopRemainingFrames);
    av_log(avctx, AV_LOG_VERBOSE, "    requiresGopRemainingFrames: %i\n",
           enc->caps.requiresGopRemainingFrames);

    enc->min_cb = 8;
    enc->min_ctb = 0;
    enc->max_cb = 0;

    /* coding blocks from 8x8 to max CTB size. */
    if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_16_BIT_EXT)
        enc->min_ctb = 16;
    else if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_32_BIT_EXT)
        enc->min_ctb = 32;
    else if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_64_BIT_EXT)
        enc->min_ctb = 64;

    /* coding blocks from 8x8 to max CTB size. */
    if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_16_BIT_EXT)
        enc->max_cb = 16;
    if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_32_BIT_EXT)
        enc->max_cb = 32;
    if (enc->caps.ctbSizes & VK_VIDEO_ENCODE_H265_CTB_SIZE_64_BIT_EXT)
        enc->max_cb = 64;

    enc->min_tbs = 0;
    enc->max_tbs = 0;
    if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_4_BIT_EXT)
        enc->min_tbs = 4;
    else if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_8_BIT_EXT)
        enc->min_tbs = 8;
    else if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_16_BIT_EXT)
        enc->min_tbs = 16;
    else if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_32_BIT_EXT)
        enc->min_tbs = 32;

    if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_4_BIT_EXT)
        enc->max_tbs = 4;
    if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_8_BIT_EXT)
        enc->max_tbs = 8;
    if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_16_BIT_EXT)
        enc->max_tbs = 16;
    if (enc->caps.transformBlockSizes & VK_VIDEO_ENCODE_H265_TRANSFORM_BLOCK_SIZE_32_BIT_EXT)
        enc->max_tbs = 32;

    av_assert0(enc->min_ctb);
    enc->surface_width  = FFALIGN(avctx->width,  enc->min_ctb);
    enc->surface_height = FFALIGN(avctx->height, enc->min_ctb);

    err = vulkan_encode_h265_init_sequence_params(avctx);
    if (err < 0)
        return err;

    err = vulkan_encode_h265_create_session(avctx);
    if (err < 0)
        return err;

    if (avctx->flags & AV_CODEC_FLAG_GLOBAL_HEADER) {
        uint8_t data[4096];
        size_t data_len = sizeof(data);

        err = vulkan_encode_h265_write_sequence_header(avctx, data, &data_len);
        if (err < 0) {
            av_log(avctx, AV_LOG_ERROR, "Failed to write sequence header "
                   "for extradata: %d.\n", err);
            return err;
        } else {
            avctx->extradata_size = data_len;
            avctx->extradata = av_mallocz(avctx->extradata_size +
                                          AV_INPUT_BUFFER_PADDING_SIZE);
            if (!avctx->extradata) {
                err = AVERROR(ENOMEM);
                return err;
            }
            memcpy(avctx->extradata, data, avctx->extradata_size);
        }
    }

    return 0;
}

static av_cold int vulkan_encode_h265_close(AVCodecContext *avctx)
{
    VulkanEncodeH265Context *enc = avctx->priv_data;
    ff_vulkan_encode_uninit(&enc->vkenc);
    return 0;
}

static int video_encode_h265_receive_packet(AVCodecContext *avctx, AVPacket *pkt)
{
    VulkanEncodeH265Context *enc = avctx->priv_data;
    return ff_vulkan_encode_receive_packet(avctx, &enc->vkenc, pkt);
}

static void vulkan_encode_h265_flush(AVCodecContext *avctx)
{

}

#define OFFSET(x) offsetof(VulkanEncodeH265Context, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM)
static const AVOption vulkan_encode_h265_options[] = {
    { "profile", "Select profile", OFFSET(vkenc.opts.profile), AV_OPT_TYPE_INT, { .i64 = FF_PROFILE_HEVC_MAIN }, 0, INT_MAX, FLAGS, "profile" },
        { "main",     NULL, 0, AV_OPT_TYPE_CONST, { .i64 = FF_PROFILE_HEVC_MAIN                }, INT_MIN, INT_MAX, FLAGS, "profile" },
        { "main10",   NULL, 0, AV_OPT_TYPE_CONST, { .i64 = FF_PROFILE_HEVC_MAIN_10             }, INT_MIN, INT_MAX, FLAGS, "profile" },
        { "rext",     NULL, 0, AV_OPT_TYPE_CONST, { .i64 = FF_PROFILE_HEVC_REXT                }, INT_MIN, INT_MAX, FLAGS, "profile" },

    { "tier", "Set tier (general_tier_flag)",
      OFFSET(tier), AV_OPT_TYPE_INT,
      { .i64 = 0 }, 0, 1, FLAGS, "tier" },
    { "main", NULL, 0, AV_OPT_TYPE_CONST,
      { .i64 = 0 }, 0, 0, FLAGS, "tier" },
    { "high", NULL, 0, AV_OPT_TYPE_CONST,
      { .i64 = 1 }, 0, 0, FLAGS, "tier" },

    { "level", "Set level (general_level_idc)",
      OFFSET(level), AV_OPT_TYPE_INT,
      { .i64 = FF_LEVEL_UNKNOWN }, FF_LEVEL_UNKNOWN, 0xff, FLAGS, "level" },

#define LEVEL(name, value) name, NULL, 0, AV_OPT_TYPE_CONST, \
      { .i64 = value }, 0, 0, FLAGS, "level"
    { LEVEL("1",    30) },
    { LEVEL("2",    60) },
    { LEVEL("2.1",  63) },
    { LEVEL("3",    90) },
    { LEVEL("3.1",  93) },
    { LEVEL("4",   120) },
    { LEVEL("4.1", 123) },
    { LEVEL("5",   150) },
    { LEVEL("5.1", 153) },
    { LEVEL("5.2", 156) },
    { LEVEL("6",   180) },
    { LEVEL("6.1", 183) },
    { LEVEL("6.2", 186) },
#undef LEVEL

    { "units", "Set units to include", OFFSET(insert_units), AV_OPT_TYPE_FLAGS, { .i64 = UNIT_AUD }, 0, INT_MAX, FLAGS, "units" },
        { "aud",        "Include AUD units", 0, AV_OPT_TYPE_CONST, { .i64 = UNIT_AUD }, INT_MIN, INT_MAX, FLAGS, "units" },

    FF_VK_ENCODE_COMMON_OPTS

    { NULL },
};

static const FFCodecDefault vulkan_encode_h265_defaults[] = {
    { "b",              "0"   },
    { "g",              "120" },
    { NULL },
};

static const AVClass vulkan_encode_h265_class = {
    .class_name = "h265_vulkan",
    .item_name  = av_default_item_name,
    .option     = vulkan_encode_h265_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

const FFCodec ff_hevc_vulkan_encoder = {
    .p.name         = "h265_vulkan",
    CODEC_LONG_NAME("H.265/HEVC (Vulkan)"),
    .p.type         = AVMEDIA_TYPE_VIDEO,
    .p.id           = AV_CODEC_ID_H265,
    .priv_data_size = sizeof(VulkanEncodeH265Context),
    .init           = &vulkan_encode_h265_init,
    FF_CODEC_RECEIVE_PACKET_CB(&video_encode_h265_receive_packet),
    .flush          = &vulkan_encode_h265_flush,
    .close          = &vulkan_encode_h265_close,
    .p.priv_class   = &vulkan_encode_h265_class,
    .p.capabilities = AV_CODEC_CAP_DELAY | AV_CODEC_CAP_HARDWARE |
                      AV_CODEC_CAP_DR1 |
                      AV_CODEC_CAP_ENCODER_FLUSH /* | AV_CODEC_CAP_EXPERIMENTAL */,
    .caps_internal  = FF_CODEC_CAP_INIT_CLEANUP,
    .defaults       = vulkan_encode_h265_defaults,
    .p.pix_fmts = (const enum AVPixelFormat[]) {
        AV_PIX_FMT_VULKAN,
        AV_PIX_FMT_NONE,
    },
    .hw_configs     = ff_vulkan_encode_hw_configs,
    .p.wrapper_name = "vulkan",
};
