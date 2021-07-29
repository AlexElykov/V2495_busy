-- Check if there is an input TRG into the V2495 module 
-- for LED calibration
-- ----------------------------------------------------

library ieee;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.V2495_pkg.all;

entity status_led is
	port(
		clk        : in std_logic;
		Pulser_in  : in std_logic;
		LED_mode   : out std_logic;
		-- Register interface          
		ctrl_regs  : in   CONTROL_REGS_T     
	);
end status_led;


architecture rtl of status_led is

signal counter			: unsigned(31 downto 0) := (others => '0');
signal max_counter	: unsigned(31 downto 0) := (others => '0');

begin

max_counter <= unsigned(ctrl_regs(0));
	
	-- Check if there was an LED NIM in the last N interval of clk cycles
	-- if there was one set LED_mode = '1'
	check_LED : process(clk)
	begin
		if rising_edge(clk) then
			if Pulser_in = '1' then
				counter <= (others => '0');
			elsif counter < max_counter then
				counter <= counter + 1;		
			end if;
			
			if counter = max_counter then
				LED_mode <= '0';
			else 
				LED_mode <= '1';
			end if;
		end if;
	
	end process check_LED;
end rtl;