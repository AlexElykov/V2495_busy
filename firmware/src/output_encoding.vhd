-- Encode the start and stop signals of a busy veto as 
-- 1 clk NIM outputs
-- ----------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.V2495_pkg.all;

entity output_encoding is
	port(
		clk         : in std_logic;
		Busy_on     : in std_logic;
		Busy_start 	: out std_logic;
		Busy_stop	: out std_logic
	);
end output_encoding;

architecture rtl of output_encoding is
	signal signal_IN_sr 	: std_logic_vector(1 downto 0);

begin
	check_state : process(clk)
	begin
		if rising_edge(clk) then
			signal_IN_sr <= signal_IN_sr(0) & Busy_on;
		end if;
	end process check_state;
	
	Busy_start 	<= '1' when signal_IN_sr = "01" else '0';
	Busy_stop	<= '1' when signal_IN_sr = "10" else '0';
	
end rtl;
