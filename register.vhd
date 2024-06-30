LIBRARY IEEE;
LIBRARY WORK;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.ALL;

ENTITY REG IS
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        data_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
END ENTITY REG;

ARCHITECTURE Behavior OF REG IS
    SIGNAL reg_val : STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');
BEGIN

    PROCESS (clk, reset)
    BEGIN
        IF reset = '1' THEN
            reg_val <= (OTHERS => '0'); -- Reset the register to 0 when reset signal is active
        ELSIF rising_edge(clk) THEN
            reg_val <= data_in; -- Load input data into the register on rising edge of the clock
        END IF;
    END PROCESS;

    -- Assign the output to the register value
    data_out <= reg_val;
END ARCHITECTURE Behavior;