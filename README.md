## Firmware for CAEN V2495 General Purpose Module

A.Elykov (AlexElykov@yahoo.com)

### Description:
Custom firmware for the UFPGA on the CAEN V2495 general purpose board


### Prerequisites:
- Altera Quartus II 13.0sp1 (web/full edition) or higher
- Cyclone V FPGA has to be supported. 
- Note: to be able to compile this on the free web version, might need to disable the SignalTap logic analyser in Assignments - Settings - SignalTap II

### Architecture: 
This is a generic busy firmware for the module and it can be easily expanded for other usages.
##### Main changes from the V1495 firmware
- Use of VHDL native libraries
- The firmware is less bloated, uses separate entities not components
- User defined registers implemented in the V2495_pkg not in the HAL
- Does not have the XENON1T/nT High Energy Veto or Veto_TRG support

### Content:
* README.md (this file)
* firmware\src
* firmware\
* .rbd – compiled firmware file


