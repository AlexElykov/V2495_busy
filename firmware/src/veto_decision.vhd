-- If there is signal in any of the LVDS inputs, and
-- issue a veto of a user defined length
-- ----------------------------------------------------

library ieee;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.V2495_pkg.all;

entity veto_decision is
	port(
		clk			: in std_logic;
		A 				: in std_logic_vector(31 downto 0);
		B				: in std_logic_vector(31 downto 0);
		Busy_on		: out std_logic;
		-- Register interface          
		ctrl_regs  : in   CONTROL_REGS_T;
		mon_regs   : out  MONITOR_REGS_T		
	);
end veto_decision;


architecture rtl of veto_decision is

signal counter			: unsigned(31 downto 0) := (others => '0');
signal max_counter	: unsigned(31 downto 0) := (others => '0');
signal ones				: unsigned(63 downto 0) := (others => '1');
signal busy_check 	: std_logic;
signal input 			: std_logic_vector(63 downto 0) := (others => '0');

begin
-- Output firmware version to read only monitor register
mon_regs(0)  <= std_logic_vector(to_unsigned(FWREV,32));  -- Firmware release version
-- output the LVDS status to dedicated registers
mon_regs(1)	<= A;      -- Port A status
mon_regs(2)	<= B;      -- Port B status

max_counter	<= unsigned(ctrl_regs(1));


-- -------------------------------------------------
input 		<= A & B;
-- Note that busy LVDS in CAEN V1724 is an active low signal 
	check_busy : process(clk) 
	begin
		if rising_edge(clk) then
			if unsigned(input) = ones then
				busy_check <= '0';
			else 
				-- trigger the counter and busy output
				busy_check <= '1';
				if counter = 0 then
					counter <= counter + 1;
				end if;
			end if;
			
			-- keep busy veto output on for user defined duration of N clk
			if busy_check = '1' or (counter > 0 and counter < max_counter) then
				Busy_on <= '1';
				if counter > 0 then
					counter <= counter + 1;
				end if;
			else 
				Busy_on <= '0';
				if counter > 0 then
					counter <= (others => '0');
				end if;
			end if;	
		end if;
	end process check_busy;
end rtl;
