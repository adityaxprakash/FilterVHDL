LIBRARY IEEE;
LIBRARY WORK;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.ALL;

ENTITY tb_mac IS
END tb_mac;

ARCHITECTURE behaviour OF tb_mac IS

    COMPONENT MAC IS
        PORT (
            clk : IN STD_LOGIC;
            we : IN STD_LOGIC;
            in1 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            in2 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            result : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
        );

    END COMPONENT;

    SIGNAL clk : STD_LOGIC;
    SIGNAL CLOCK_PERIOD : TIME := 10ns;
    SIGNAL outp : STD_LOGIC_VECTOR(15 DOWNTO 0);
    SIGNAL input1 : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL input2 : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL enab : STD_LOGIC := '1';
    SIGNAL wr_comp : INTEGER := 0;
    SIGNAL i : INTEGER := 1;

BEGIN
    clk_process : PROCESS
    BEGIN
        clk <= '1';
        WAIT FOR CLOCK_PERIOD/2;
        clk <= '0';
        WAIT FOR CLOCK_PERIOD/2;
    END PROCESS clk_process;

    uut : MAC PORT MAP(
        in1 => input1,
        in2 => input2,
        result => outp,
        clk => clk,
        we => enab
    );

    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (i < 6) THEN
                enab <= '1';
                input1 <= "00000100";
                input2 <= STD_LOGIC_VECTOR(to_signed(i, 8));
                i <= i + 1;
            ELSE
                enab <= '0';
            END IF;
        END IF;
    END PROCESS;
END behaviour;