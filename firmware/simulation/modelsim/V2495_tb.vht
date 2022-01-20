
-- Test Bench for a V2495 busy module
-- Simulation tool : ModelSim-Altera (VHDL)
-- 
--

library ieee;                                               
use ieee.std_logic_1164.all;                                
use ieee.numeric_std.all;
-- add the global ctrl signal package to testbench
use IEEE.std_logic_unsigned.all;  
use work.V2495_pkg.all;

use ieee.std_logic_textio.all;
use std.textio.all;



ENTITY V2495_vhd_tst IS
END V2495_vhd_tst;


ARCHITECTURE V2495_arch OF V2495_vhd_tst IS
	
-- Declare internal signals to trigger and assign default values                                                  
signal A 	: STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '1');
signal B 	: STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '1');
signal C 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
signal CLK 	: STD_LOGIC;
signal D 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
signal E 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
signal F 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
signal GD_DELAYED : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal GD_START : STD_LOGIC_VECTOR(31 DOWNTO 0);
signal GIN 	: STD_LOGIC_VECTOR(1 DOWNTO 0) := "00";	
signal GOUT : STD_LOGIC_VECTOR(1 DOWNTO 0);
signal IDD 	: STD_LOGIC_VECTOR(2 DOWNTO 0);
signal IDE 	: STD_LOGIC_VECTOR(2 DOWNTO 0);
signal IDF 	: STD_LOGIC_VECTOR(2 DOWNTO 0);
signal LAD 	: STD_LOGIC_VECTOR(15 DOWNTO 0);
signal LED 	: STD_LOGIC_VECTOR(7 DOWNTO 0);
signal nADS : STD_LOGIC;
signal nBLAST : STD_LOGIC;
signal nINT : STD_LOGIC;

-- reset for local bus interface
signal nLBRES : STD_LOGIC := '0'; 

signal nOED : STD_LOGIC;
signal nOEE : STD_LOGIC;
signal nOEF : STD_LOGIC;
signal nOEG : STD_LOGIC;
signal nREADY : STD_LOGIC;
signal SELD : STD_LOGIC;
signal SELE : STD_LOGIC;
signal SELF : STD_LOGIC;
signal SELG : STD_LOGIC;
signal SPI_CS : STD_LOGIC;
signal SPI_MISO : STD_LOGIC;
signal SPI_MOSI : STD_LOGIC;
signal SPI_SCLK : STD_LOGIC;
signal WnR 	: STD_LOGIC;	
	

-- User test signals
-------------------------------------------------------
-- just a quick test for the trigger input
signal gin_i : std_logic_vector(2 downto 0) := "001";
signal tt : std_logic;	
signal res : std_logic := '1';


-- Define the period of the CLK clock
constant CLK_PERIOD : time := 20 ns;

-- Input to mimic LVDS input from busy
-- Busy LVDS in CAEN V17XX is an active low signal 
signal A_LVDS : std_logic_vector(31 downto 0) := (0 => '0', others => '1');
signal B_LVDS : std_logic_vector(31 downto 0) := (0 => '1', others => '1');

-- Set registers
signal mon_regs_tb    : MONITOR_REGS_T;
signal ctrl_regs_tb   : CONTROL_REGS_T;

-- Input into the local bus interface
constant N_regs : integer := 4; 				-- Number of mapped control regs 

-- setup the arrays to be sent to the bus
type ctr_reg_addr is ARRAY (0 to N_regs - 1) of std_logic_vector(15 downto 0);  
signal ctr_reg_sup : ctr_reg_addr := (x"1800", x"1804", x"1808", x"180C");

type ctr_reg_dat is ARRAY (0 to N_regs -1) of std_logic_vector(15 downto 0);  
signal ctr_reg_dt : ctr_reg_dat := (x"0100", x"0010", x"0600", x"0900");
	
begin

-- Instantiate the Unit Under Test (UUT)
-------------------------------------------------------
uut : entity work.V2495(rtl) 
	port map(
		A => A,
		B => B,
		C => C,
		CLK => CLK,
		D => D,
		E => E,
		F => F,
		GD_DELAYED => GD_DELAYED,
		GD_START => GD_START,
		GIN => GIN,
		GOUT => GOUT,
		IDD => IDD,
		IDE => IDE,
		IDF => IDF,
		LAD => LAD,
		LED => LED,
		nADS => nADS,
		nBLAST => nBLAST,
		nINT => nINT,
		nLBRES => nLBRES,
		nOED => nOED,
		nOEE => nOEE,
		nOEF => nOEF,
		nOEG => nOEG,
		nREADY => nREADY,
		SELD => SELD,
		SELE => SELE,
		SELF => SELF,
		SELG => SELG,
		SPI_CS => SPI_CS,
		SPI_MISO => SPI_MISO,
		SPI_MOSI => SPI_MOSI,
		SPI_SCLK => SPI_SCLK,
		WnR => WnR
	);

	
	
	
-- Clock driver for the 50 MHz clock
-------------------------------------------------------
clk_process : process
	begin
		CLK <= '0';
		wait for CLK_PERIOD/2;  
		CLK <= '1';
		wait for CLK_PERIOD/2; 
end process;



-- Local bus initiaton, to write registers
-- to monitor local_bus process variablesin ModelSim add them via: View -> Locals
-- expalined here: https://www.nandland.com/vhdl/tips/tip-viewing-variables-in-modelsim.html
-------------------------------------------------------
init_bus : process
	begin 
		
		-- Writing to the bus goes as: LBIDLE -> LBWRITEL -> LBWRITEH -> LBIDLE
		-- Supply address (0_15) into LAD (on LBIDLE), 
		-- Supply the data (0_15) into LAD (on LBWRITEL)
		-- Data is written on LBWRITEH
		-- Next address is supplied on LBIDLE, 2 clk cycles after LBWRITEL
		wait for CLK_PERIOD * 4;	
		
		nLBRES <= '1';						-- Generating a reset at the beginning of the simulation
		nADS <= '0'; 						-- Start address sampling cycle (active low)
		WnR <= '1';							-- Init write access	
		nBLAST <= '1';						-- Allow data transfer (active low) put machine back into idle state
	

		for i in 0 to N_regs - 1 loop
		
			LAD <=  ctr_reg_sup(i);
			
			wait for CLK_PERIOD;
			
			LAD <= ctr_reg_dt(i);
			
			wait for CLK_PERIOD * 2;
			
		end loop;
	
--		LAD <= x"1800";  
--		wait for CLK_PERIOD;
--		LAD <= x"0100"; 
--		wait for CLK_PERIOD * 2;
--		LAD <= x"1804";  
--		wait for CLK_PERIOD;
--		LAD <= x"0300";
--		wait for CLK_PERIOD * 2;
--		LAD <= x"1808";  
--		wait for CLK_PERIOD;
--		LAD <= x"0600";
		
	
		wait for 200 ns;
		LAD <= (others => '0');
		
		-- Afterwards keep the bus readout idle, to keep registers stored in their place
		nBLAST <= '0'; 			-- End data transfer (active low) put machine back into idle state
		nADS <= '1'; 				-- End address sampling cycle (active low)
		WnR <= '0';
		
		-- reset everything
		--nLBRES <= '0';  			-- reset and put local bus into idle state
		wait;
		
end process;




-- Triggering the busy LVDS input into V2495
-------------------------------------------------------
shift_reg : process
	begin
---- can use shift register to loop over all LVDS inputs		
--		-- loop from from 1 to 30
--		for i in A_LVDS'left downto A_LVDS'right + 1 loop
--			A_LVDS(i) <= A_LVDS(i - 1);
--			
--			A <= A_LVDS;
--			wait for CLK_PERIOD;
--		end loop;
--		-- reset the bit position
--		A_LVDS(A_LVDS'right) <= A_LVDS(A_LVDS'left);
						
		wait for 10 us;
		A_LVDS <= (others => '1');
		A <= A_LVDS;
 
 		wait for 20 us;
		A_LVDS <= (0 => '0', others => '1');
		A <= A_LVDS;
		
end process;


-- Trigger external NIM input into the V2495
-- mimicing an external LED trigger
-------------------------------------------------------
trg_test : process
	begin
--		for g in gin_i'left downto gin_i'right + 1 loop
--			
--			gin_i(g) <= gin_i(g- 1);
--			wait for 200 ns;
--		end loop;
--		gin_i(gin_i'right) <= gin_i(gin_i'left);

		GIN <= "00"; --gin_i(2 downto 1);	
		wait for 10 us;
		
		GIN <= "10";
		wait for 15 us;
		
		GIN <= "00";
		wait for 25 us;
		
		GIN <= "10";
		wait for 35 us;
		
		GIN <= "00";
		wait;
		
end process;


-- Accessing signals from the top entity via a path_name
-- (works only in simulation and testbench needs to be compiled with vhdl 2008 in ModelSim)
-------------------------------------------------------
myblock : block 
    alias tt_alias is << signal .V2495_vhd_tst.uut.TRG_LED : std_logic >>;
  begin
     tt <= << signal .V2495_vhd_tst.uut.TRG_LED : std_logic >>;
	  
end block myblock ; 



-- Write output signals to file
-------------------------------------------------------
file_dump : process(CLK)

	file output_file : text open write_mode is "output_waves.txt";
	variable row_out : line;
	
	begin 
		if rising_edge(CLK) then
		
		-- incoming waveform	
		write(row_out, to_integer(signed(not(A_LVDS))), right, 16);
	
		-- hex write
		--hwrite(row_out, F, right, 6);
		
		-- veto output NIMs
		write(row_out, F(7), right, 1);
		write(row_out, F(6), right, 1);
		write(row_out, F(5), right, 1);
		write(row_out, F(4), right, 1);
		
		-- ext LED TRG
		write(row_out, GIN(1), right, 1);
		
		writeline(output_file, row_out);
	end if;

end process file_dump;


END V2495_arch;


