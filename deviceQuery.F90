program deviceQuery
use cudafor
implicit none
type(cudaDeviceProp) :: prop
integer::nDevices=0,i,ierr

ierr=cudaGetDeviceCount(nDevices)
if(nDevices==0)then
  write(*,"(/,'No CUDA devices found',/)")
  stop
elseif(nDevices==1)then
  write(*,"(/,'One CUDA device found',/)")
else
  write(*,"(/,i0,'CUDA devices found',/)") nDevices
endif

do i=0,nDevices-1
  write(*,"('DeviceNumber:',i0)")i
  ierr=cudaGetDeviceProperties(prop,i)
  write(*,"('DeviceName                 :',a)")trim(prop%name)
  write(*,"('ComputeCapability          :',i0,'.',i0)") prop%major,prop%minor
  write(*,"('NumberofMultiprocessors    :',i0)") prop%multiProcessorCount
  write(*,"('WarpSize                   :',i0)") prop%warpSize 
  write(*,"('MaxThreadsperMultiprocessor:',i0)") prop%maxThreadsPerMultiprocessor
  write(*,"('GlobalMemory(GB)           :',f9.3,/)") prop%totalGlobalMem/1024.0**3
  write(*,"('ExecutionConfigurationLimits')")
  write(*,"('MaxGridDims                :',2(i0,'x'),i0)") prop%maxGridSize
  write(*,"('MaxBlockDims               :',2(i0,'x'),i0)") prop%maxThreadsDim
  write(*,"('MaxThreadsperBlock         :',i0,/)") prop%maxThreadsPerBlock
enddo

endprogramdeviceQuery
