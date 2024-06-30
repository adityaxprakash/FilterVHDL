LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY VGA_Sync_background IS
    PORT (
        clk : IN STD_LOGIC;
        reset : IN STD_LOGIC;
        hsync : OUT STD_LOGIC;
        vsync : OUT STD_LOGIC;
        video_on : OUT STD_LOGIC;
        p_tick : OUT STD_LOGIC;
        addr_to_print : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
        --         HCOU: out std_logic_vector( 9 downto 0);
        --         VCOU:Out std_logic_vector(9 downto 0)
    );
END ENTITY VGA_Sync_background;

ARCHITECTURE Behavioral OF VGA_Sync_background IS
    SIGNAL p_reg : INTEGER := 0;
    SIGNAL pix_tick : STD_LOGIC;
    SIGNAL HCOUNT : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');
    SIGNAL VCOUNT : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');
    SIGNAL vsync_register : STD_LOGIC;
    SIGNAL hsync_register : STD_LOGIC;
BEGIN
    clock_divider : PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (p_reg = 0) THEN
                pix_tick <= '1';
                p_reg <= 1;
            ELSIF (p_reg = 1) THEN
                p_reg <= 2;
            ELSIF (p_reg = 2) THEN
                pix_tick <= '0';
                p_reg <= 3;
            ELSE
                p_reg <= 0;
            END IF;
        END IF;
    END PROCESS;

    hcount_vcount_process : PROCESS (clk, reset, pix_tick)
    BEGIN
        IF reset = '1' THEN
            VCOUNT <= (OTHERS => '0');
            HCOUNT <= (OTHERS => '0');
            vsync_register <= '0';
            hsync_register <= '0';
        ELSIF rising_edge(pix_tick) THEN
            --             if pix_tick = '1' then
            IF HCOUNT = 799 THEN
                HCOUNT <= (OTHERS => '0');
                hsync_register <= '0';
                IF VCOUNT = 524 THEN
                    VCOUNT <= (OTHERS => '0');
                    vsync_register <= '0';
                ELSE
                    VCOUNT <= VCOUNT + 1;
                    IF VCOUNT >= 512 AND VCOUNT <= 513 THEN
                        vsync_register <= '1';
                    ELSE
                        vsync_register <= '0';
                    END IF;
                END IF;
            ELSE
                HCOUNT <= HCOUNT + 1;
                VCOUNT <= VCOUNT;
                IF HCOUNT >= 655 AND HCOUNT <= 750 THEN
                    hsync_register <= '1';
                ELSE
                    hsync_register <= '0';
                END IF;
                --             end if;
            END IF;

        END IF;
    END PROCESS;
    videon : PROCESS (HCOUNT, VCOUNT, clk)
        VARIABLE counter : INTEGER := 0;
    BEGIN
        IF (rising_edge(clk)) THEN
            IF HCOUNT >= 192 AND HCOUNT <= 255 AND VCOUNT >= 112 AND VCOUNT <= 175 THEN
                --        if(counter=0) then
                --            counter=counter+1;
                video_on <= '1';
            ELSE
                video_on <= '0';
            END IF;
        END IF;
    END PROCESS;

    output_handling : PROCESS (clk)

        VARIABLE actual_h : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');
        VARIABLE actual_v : STD_LOGIC_VECTOR(9 DOWNTO 0) := (OTHERS => '0');
    BEGIN
        IF (rising_edge(clk)) THEN
            IF (HCOUNT >= 192 AND HCOUNT <= 255 AND VCOUNT >= 112 AND VCOUNT <= 175) THEN
                actual_h := HCOUNT - 192;
                actual_v := VCOUNT - 112;
                addr_to_print <= actual_v(5 DOWNTO 0) & actual_h(5 DOWNTO 0);
                --        if( HCOUNT<64 and VCOUNT<64) then
                --            actual_h := HCOUNT ;
                --            actual_v := VCOUNT;
                --            addr_to_print<= actual_v(5 downto 0)&actual_h(5 downto 0);
            ELSE
                addr_to_print <= (OTHERS => '0');
            END IF;
            hsync <= hsync_register;
            vsync <= vsync_register;
            p_tick <= pix_tick;
            --             HCOU<=HCOUNT;
            --             VCOU<=VCOUNT;
        END IF;
    END PROCESS;

END ARCHITECTURE Behavioral;