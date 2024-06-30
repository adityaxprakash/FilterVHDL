LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
ENTITY tb_ROM_block IS
END tb_ROM_block;
ARCHITECTURE behavior OF tb_ROM_block IS
    COMPONENT image
        PORT (
            clka : IN STD_LOGIC;
            addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT;
    --Inputs
    SIGNAL clock : STD_LOGIC := '0';
    SIGNAL rdaddress : STD_LOGIC_VECTOR(11 DOWNTO 0);
    --Outputs
    SIGNAL data : STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');
    -- Clock period definitions
    CONSTANT clock_period : TIME := 10 ns;
    SIGNAL i : INTEGER := 0;
    SIGNAL counter : INTEGER := 0;
BEGIN
    -- Read image in VHDL
    uut : image PORT MAP(
        clka => clock,
        douta => data,
        addra => rdaddress
    );
    -- Clock process definitions
    clock_process : PROCESS
    BEGIN
        clock <= '0';
        WAIT FOR clock_period/2;
        clock <= '1';
        WAIT FOR clock_period/2;
    END PROCESS;
    -- Stimulus process
    stim_proc : PROCESS (clock)
    BEGIN
        IF (rising_edge(clock)) THEN
            IF (i < 4096) THEN
                rdaddress <= STD_LOGIC_VECTOR(to_unsigned(i, 12));
                IF (counter = 0) THEN
                    counter <= counter + 1;
                ELSIF (counter = 1) THEN
                    counter <= counter + 1;
                ELSIF (counter = 2) THEN
                    counter <= counter + 1;
                ELSE
                    counter <= 0;
                    i <= i + 1;
                END IF;
            END IF;
        END IF;
    END PROCESS;
END;