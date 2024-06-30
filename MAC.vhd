LIBRARY IEEE;
LIBRARY WORK;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.ALL;

ENTITY MAC IS
    PORT (
        clk : IN STD_LOGIC;
        we : IN STD_LOGIC;
        in1 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        in2 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        result : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
    );
END MAC;

ARCHITECTURE behaviour OF MAC IS
    SIGNAL finalresult : INTEGER := 0;
    SIGNAL temp : INTEGER := 0;
BEGIN
    temp <= to_integer(unsigned(in1)) * to_integer(signed(in2));
    result <= STD_LOGIC_VECTOR(to_signed(finalresult, 16));
    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (we = '1') THEN
                finalresult <= finalresult + temp;
            ELSE
                finalresult <= temp;
            END IF;
        END IF;
    END PROCESS;
END behaviour;