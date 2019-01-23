#!/bin/bash

# --profile-api-trace all \
# --concurrent-kernels on \
# --profile-from-start on \
# --system-profiling off \
# --unified-memory-profiling per-process-device \
# --profile-child-processes \
# --openacc-profiling on \
# --aggregate-mode off \
# --source-level-analysis branch,shared_access,global_access \
# --metrics sm_efficiency \
# --dump-instruction-correlation \
# --kernels 1:7:run_simple4_actke_:1 \
# --devices 0 \
# --metrics gld_throughput,flop_count_dp_mul,gst_throughput,flop_count_sp_fma,flop_count_sp_add,flop_count_sp_special,flop_count_sp_mul,dram_read_throughput,inst_executed,dram_write_throughput,tex_cache_throughput,dram_utilization,tex_utilization,local_load_throughput,shared_store_transactions,local_store_throughput,shared_load_throughput,local_load_transactions,shared_store_throughput,shared_load_transactions,gld_transactions,local_store_transactions,tex_cache_transactions,cf_fu_utilization,gst_transactions,tex_fu_utilization,ldst_fu_utilization,double_precision_fu_utilization,dram_read_transactions,special_fu_utilization,single_precision_fu_utilization,dram_write_transactions,flop_count_dp_add,flop_count_dp_fma,achieved_occupancy,atomic_transactions,ldst_issued,warp_execution_efficiency,issue_slot_utilization,warp_nonpred_execution_efficiency,inst_fp_32,inst_fp_64,shared_utilization,inst_control,inst_compute_ld_st,inst_integer,inst_bit_convert,issue_slots,cf_issued,inst_misc,inst_inter_thread_communication,local_memory_overhead,l2_tex_read_transactions,l2_tex_write_transactions,l2_tex_write_throughput,sysmem_read_throughput,sysmem_write_throughput,l2_read_throughput,l2_write_throughput,l2_atomic_transactions,sysmem_utilization,l2_utilization,l2_atomic_throughput,stall_memory_dependency,stall_exec_dependency,stall_inst_fetch,stall_constant_memory_dependency,stall_other,stall_sync,stall_texture,sysmem_read_transactions,stall_not_selected,stall_memory_throttle,stall_pipe_busy,ecc_transactions,l2_write_transactions,l2_read_transactions,sysmem_write_transactions,sysmem_read_utilization,ecc_throughput,sysmem_write_utilization \

# ,issue_slot_utilization,warp_nonpred_execution_efficiency,inst_fp_32,inst_fp_64,shared_utilization,inst_control,inst_compute_ld_st,inst_integer,inst_bit_convert,issue_slots,cf_issued,inst_misc,inst_inter_thread_communication,local_memory_overhead,l2_tex_read_transactions,l2_tex_write_transactions,l2_tex_write_throughput,sysmem_read_throughput,sysmem_write_throughput,l2_read_throughput,l2_write_throughput,l2_atomic_transactions,sysmem_utilization,l2_utilization,l2_atomic_throughput,stall_memory_dependency,stall_exec_dependency,stall_inst_fetch,stall_constant_memory_dependency,stall_other,stall_sync,stall_texture,sysmem_read_transactions,stall_not_selected,stall_memory_throttle,stall_pipe_busy,ecc_transactions,l2_write_transactions,l2_read_transactions,sysmem_write_transactions,sysmem_read_utilization,ecc_throughput,sysmem_write_utilization \
/usr/local/cuda/cuda-8.0/bin/nvprof \
  --metrics gld_throughput,flop_count_dp_mul,gst_throughput,flop_count_sp_fma,flop_count_sp_add,flop_count_sp_special,flop_count_sp_mul,dram_read_throughput,inst_executed,dram_write_throughput,tex_cache_throughput,dram_utilization,tex_utilization,local_load_throughput,shared_store_transactions,local_store_throughput,shared_load_throughput,local_load_transactions,shared_store_throughput,shared_load_transactions,gld_transactions,local_store_transactions,tex_cache_transactions,cf_fu_utilization,gst_transactions,tex_fu_utilization,ldst_fu_utilization,double_precision_fu_utilization,dram_read_transactions,special_fu_utilization,single_precision_fu_utilization,dram_write_transactions,flop_count_dp_add,flop_count_dp_fma,achieved_occupancy,atomic_transactions,ldst_issued,warp_execution_efficiency \
  --device-buffer-size 8 \
  --device-cdp-buffer-size 8 \
  --aggregate-mode on \
  -o /tmp/api_%p.log0 \
  --log-file-verbose /tmp/nvprof_%p.log \
  /home/visit/pmargui/actke/simple4/main_simple4_actke.GPU.x


