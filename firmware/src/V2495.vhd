-- Busy firmware for a V2495-based veto module 
-- -----------------------------------------------------------------------
-- V2495 top level
-- -----------------------------------------------------------------------
--  Date        : July 2021

--  Note: to be able to compile this, just in case disable the SignalTap
--  logic analyser in Assignments->Settings->SignalTap II

-- -----------------------------------------------------------------------
-- This is a generic busy firmware for the general CAEN V2495 module.
-- It can be easily expanded for other usages.  

-- Changes compared to the XENON1T/nT V1495 firmware
-- -----------------------------------------------------------------------
-- Use of VHDL native libraries
-- The firmware is less bloated, uses separate entities not components
-- User defined registers implemented in the V2495_pkg not in the HAL
-- Does not have the XENON1T/nT High Energy Veto or Veto_TRG support


-- Currently used registers
-- -----------------------------------------------------------------------
-- # Control registers 
--  ctrl_regs(0) - 0x1800 - R/W - Set the maximum TRG input window 
--  ctrl_regs(1) - 0x1804 - R/W - Set the maximum veto duration N clk
--  ctrl_regs(2) - 0x1808 - R/W - 
--  ctrl_regs(3) - 0x180C - R/W - 
-- # Monitor registers 
--  mon_regs(0) - 0x1000 - R - Firmware version  
--  mon_regs(1) - 0x1004 - R - Status of LVDS port A  
--  mon_regs(2) - 0x1008 - R - Value of LVDS port B  
--  mon_regs(3) - 0x100C - R - 


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.V2495_pkg.all;

-- ----------------------------------------------
entity V2495 is
-- ----------------------------------------------
    port (

        CLK    : in     std_logic;                         -- System clock (50 MHz)
    -- Mainboard I/O ports
    -- ------------------------------------------------------   
      -- Port A : 32-bit LVDS/ECL input
         A        : in    std_logic_vector (31 DOWNTO 0);  -- Data bus 
      -- Port B : 32-bit LVDS/ECL input                    
         B        : in    std_logic_vector (31 DOWNTO 0);  -- Data bus
      -- Port C : 32-bit LVDS output                       
         C        : out   std_logic_vector (31 DOWNTO 0);  -- Data bus
      -- Port G : 2 NIM/TTL input/output NIM(0)/TTL(1)
         GIN      : in    std_logic_vector ( 1 DOWNTO 0);  -- In data
         GOUT     : out   std_logic_vector ( 1 DOWNTO 0);  -- Out data
         SELG     : out   std_logic;                       -- Level select
         nOEG     : out   std_logic;                       -- Output Enable

    -- Expansion slots
    -- ------------------------------------------------------                                                                  
      -- PORT D Expansion control signals                  
         IDD      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELD     : out   std_logic;                       -- Level select
         nOED     : out   std_logic;                       -- Output Enable
         D        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus
                                                           
      -- PORT E Expansion control signals                  
         IDE      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELE     : out   std_logic;                       -- Level select
         nOEE     : out   std_logic;                       -- Output Enable
         E        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus
                                                           
      -- PORT F Expansion control signals                  
         IDF      : in    std_logic_vector ( 2 DOWNTO 0);  -- Card ID
         SELF     : out   std_logic;                       -- Level select
         nOEF     : out   std_logic;                       -- Output Enable
         F        : inout std_logic_vector (31 DOWNTO 0);  -- Data bus

    -- Gate & Delay - not used in this firmware
    -- ------------------------------------------------------
      --G&D I/O
        GD_START   : out  std_logic_vector(31 downto 0);   -- Start of G&D
        GD_DELAYED : in   std_logic_vector(31 downto 0);   -- G&D Output
      --G&D SPI bus                                        
        SPI_MISO   : in   std_logic;                       -- SPI data in
        SPI_SCLK   : out  std_logic;                       -- SPI clock
        SPI_CS     : out  std_logic;                       -- SPI chip sel.
        SPI_MOSI   : out  std_logic;                       -- SPI data out
      
    -- User LED
    -- ------------------------------------------------------
        LED        : out std_logic_vector(7 downto 0);     -- User led    
    
    -- Local Bus in/out signals
    -- ------------------------------------------------------
      -- Communication interface
        nLBRES     : in     std_logic;                     -- Bus reset
        nBLAST     : in     std_logic;                     -- Last cycle
        WnR        : in     std_logic;                     -- Read (0)/Write(1)
        nADS       : in     std_logic;                     -- Address strobe
        nREADY     : out    std_logic;                     -- Ready (active low) 
        LAD        : inout  std_logic_vector (15 DOWNTO 0);-- Address/Data bus
      -- Interrupt requests  
        nINT       : out    std_logic                      -- Interrupt request
  );
end V2495;

-- -------------------------------------------------------
architecture rtl of V2495 is

	 -- User temp signals
	signal Busy_on		: std_logic;
	signal TRG_LED		: std_logic;
	signal LED_mode 	: std_logic;
	signal Veto_out	: std_logic;
	signal Busy_stop	: std_logic;
	signal Busy_start	: std_logic;

	 
	-- Set registers
   signal mon_regs    : MONITOR_REGS_T;
   signal ctrl_regs   : CONTROL_REGS_T;

   -- Gate & Delay control bus signals
   signal gd_write     :  std_logic;
   signal gd_read      :  std_logic;
   signal gd_ready     :  std_logic;
   signal reset        :  std_logic;
   signal gd_data_wr   :  std_logic_vector(31 downto 0);
   signal gd_data_rd   :  std_logic_vector(31 downto 0);
   signal gd_command   :  std_logic_vector(15 downto 0);
          
begin -- -------------------------------------------------

	-- Set the level and direction of the used ports
	-- noEX = set output(0), input(1) 
	-- SELX = set level NIM(0), TTL(1)
	
	-- Ports G are input NIM 
	nOEG 	<= '1';
	SELG	<= '0';
	-- Ports E, F are NIM outputs
	SELE 	<= '0';
   nOEE 	<= '0';
	SELF 	<= '0';
   nOEF 	<= '0';

	-- Unused output ports are explicitally set to HiZ
	-- ----------------------------------------------------
   GOUT <= (others => 'Z');
   SELD <= 'Z';
   nOED <= 'Z';
   D    <= (others => 'Z');
   GD_START <= (others => 'Z');
	-- Set the E ports unused for now
	E    <= (others => 'Z');

   -- Local bus Interrupt request
   nINT <= '1';
   
   -- User Led driver
   -- LED <= std_logic_vector(to_unsigned(DEMO_NUMBER,8)); 
   reset <= not(nLBRES);
    
	 
   -- Check if any of the digitizers are busy
	-- ----------------------------------------------------
   I_VETO_DECISION: entity work.veto_decision  
		port map (
			clk         => CLK,   
			-- Register interface  
         ctrl_regs   => ctrl_regs,
			mon_regs    => mon_regs,
			-- check if there is an LVDS busy
			A				=> A,
			B				=> B,
			Busy_on		=> Busy_on
        );

   -- Encode start and stop of busy veto as 1 clk NIM
	-- ----------------------------------------------------
   I_OUT_ENCODE: entity work.output_encoding  
		port map (
			clk         => CLK,   
			-- Start and stop signals
			Busy_on 		=> Busy_on,
			Busy_start 	=> Busy_start,
			Busy_stop	=> Busy_stop
        );
		  
   -- Check if there is an external trigger 
	-- ----------------------------------------------------
   I_STATUS_LED: entity work.status_led
		port map (
			clk         => CLK,
			-- Register interface  
			ctrl_regs   => ctrl_regs,
			-- LED input mode check
			Pulser_in	=> GIN(1),
			LED_mode		=> LED_mode
		);
	 
   --  Local Bus slave interface
	-- ----------------------------------------------------
   I_LBUS_INTERFACE: entity work.lb_int
		port map (
			clk         => CLK,
			reset       => reset,
         -- Local Bus            
         nBLAST      => nBLAST,   
         WnR         => WnR,      
         nADS        => nADS,
			nREADY      => nREADY,
			LAD         => LAD,
			-- Register interface  
         ctrl_regs   => ctrl_regs,
			mon_regs    => mon_regs,
			-- Gate and Delay controls
			gd_data_wr  => gd_data_wr,       
         gd_data_rd  => gd_data_rd,         
         gd_command  => gd_command,
         gd_write    => gd_write,
         gd_read     => gd_read,
         gd_ready    => gd_ready
		);
		  
	-- ----------------------------------------------------
		set_outputs: process(clk)
		begin
			if rising_edge(clk) then
				-- If there is a busy output a busy veto
				if Busy_on = '1' and LED_mode = '0' then 
					Veto_out <= '1';	
				-- If there is no busy detected propagate the pulser
				elsif Busy_on = '0' then 
					TRG_LED 	<= GIN(1);
				else 
					Veto_out <= '0';
				end if;
			end if;
		end process set_outputs;
		
	-- ----------------------------------------------------

		-- Hardware outputs
		F(0)		<= TRG_LED;
		F(1)		<= Veto_out;
		F(2)		<= Busy_start;
		F(3)		<= Busy_stop;
		
		-- User LED outputs
		LED(0)	<= Veto_out;
		LED(1)	<= LED_mode;

end rtl;
   