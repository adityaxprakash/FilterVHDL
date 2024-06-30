LIBRARY IEEE;
LIBRARY WORK;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.ALL;

ENTITY fsm IS
    PORT (
        clock : IN STD_LOGIC; -- change on basis board
        trigger : IN STD_LOGIC; -- map this on the basis board
        reset : IN STD_LOGIC; -- mapped on basis
        Hsync : OUT STD_LOGIC;
        Vsync : OUT STD_LOGIC;
        mode : IN STD_LOGIC;
        rgb : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
    );

END fsm;

ARCHITECTURE behaviour OF fsm IS

    COMPONENT image_rom
        PORT (
            clka : IN STD_LOGIC;
            addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT filter
        PORT (
            clka : IN STD_LOGIC;
            addra : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT final_image
        PORT (
            clka : IN STD_LOGIC;
            wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
            addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
            dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT MAC IS
        PORT (
            clk : IN STD_LOGIC;
            we : IN STD_LOGIC;    
            in1 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            in2 : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            result : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
        );
    END COMPONENT;

    -- new component
    COMPONENT VGA_Sync_background IS
        PORT (
            clk : IN STD_LOGIC;
            reset : IN STD_LOGIC;
            hsync : OUT STD_LOGIC;
            vsync : OUT STD_LOGIC;
            video_on : OUT STD_LOGIC;
            p_tick : OUT STD_LOGIC;
            addr_to_print : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
        );
    END COMPONENT;

    TYPE state_type IS (idle, processing, display);
    TYPE process_state_type IS (read_rom, calculate, cal_min_max, write_ram, go_next);

    SIGNAL filter_position : INTEGER := 0;
    SIGNAL i : INTEGER := 0;
    SIGNAL j : INTEGER := 0;

    SIGNAL state : state_type := idle;
    SIGNAL process_state : process_state_type;

    SIGNAL write_enable_ram : STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";
    SIGNAL ram_address : STD_LOGIC_VECTOR(11 DOWNTO 0);
    SIGNAL ram_data_in : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL ram_data : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL rom_address : STD_LOGIC_VECTOR(11 DOWNTO 0);
    SIGNAL rom_data : STD_LOGIC_VECTOR(7 DOWNTO 0);

    SIGNAL write_enable_mac : STD_LOGIC := '0';
    SIGNAL input_1_mac : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL input_2_mac : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL data_out_mac : STD_LOGIC_VECTOR(15 DOWNTO 0);

    SIGNAL filter_val : STD_LOGIC_VECTOR(7 DOWNTO 0);
    SIGNAL filter_loc : STD_LOGIC_VECTOR(3 DOWNTO 0);

    SIGNAL stop_reg : STD_LOGIC := '0';
    SIGNAL clock_cycle : INTEGER := 0;
    SIGNAL check : STD_LOGIC := '0';

    -- new signals
    SIGNAL video_on : STD_LOGIC;
    SIGNAL rdaddress : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL newclk : STD_LOGIC;
    SIGNAL rgb_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

    --normaling the image
    SIGNAL min : INTEGER RANGE -1000 TO 1000 := 999;
    SIGNAL max : INTEGER RANGE -1000 TO 1000 := - 999;
    SIGNAL min_max_done : STD_LOGIC := '0';
    SIGNAL pixel : INTEGER := 0;
    SIGNAL diff : INTEGER := 0;
BEGIN

    mac_unit : MAC PORT MAP(
        clk => clock,
        we => write_enable_mac,
        in1 => input_1_mac,
        in2 => input_2_mac,
        result => data_out_mac
    );

    input_image : image_rom PORT MAP(--this is correct
        douta => rom_data,
        clka => clock,
        addra => rom_address);

    filter2 : filter PORT MAP(
        douta => filter_val,
        clka => clock,
        addra => filter_loc
    );
    ram : final_image PORT MAP(
        douta => ram_data,
        clka => clock,
        dina => ram_data_in,
        addra => ram_address,
        wea => write_enable_ram
    );

    vga_sync_unit : VGA_Sync_background
    PORT MAP(
        clk => clock,
        reset => reset,
        hsync => Hsync,
        vsync => Vsync,
        video_on => video_on,
        p_tick => newclk,
        addr_to_print => rdaddress
    );

    PROCESS (clock)
    BEGIN

        IF rising_edge (clock) THEN
            IF state = idle THEN
                IF trigger = '1' THEN
                    state <= processing;
                END IF;
            ELSIF state = processing THEN
                IF stop_reg = '1' THEN
                    state <= display;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    -- substate process + display process
    PROCESS (clock)
        VARIABLE pix_val : INTEGER := 0;
    BEGIN
        IF rising_edge (clock) THEN

            IF (state = processing) THEN
                IF (process_state = read_rom) THEN --  addresses of input & output are updated
                    write_enable_ram <= "0";
                    write_enable_mac <= '1';

                    input_1_mac <= (OTHERS => '0');
                    input_2_mac <= (OTHERS => '0');

                    IF (filter_position = 0) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i > 0 AND j > 0) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i - 1) * 64 + j - 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 1) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i > 0) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i - 1) * 64 + j, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 2) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i > 0 AND j < 63) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i - 1) * 64 + j + 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 3) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (j > 0) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned(i * 64 + j - 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 4) THEN

                        rom_address <= STD_LOGIC_VECTOR(to_unsigned(i * 64 + j, 12));
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));

                    ELSIF (filter_position = 5) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (j < 63) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned(i * 64 + j + 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 6) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i < 63 AND j > 0) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i + 1) * 64 + j - 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 7) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i < 63) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i + 1) * 64 + j, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    ELSIF (filter_position = 8) THEN
                        filter_loc <= STD_LOGIC_VECTOR(to_unsigned(filter_position, 4));
                        IF (i < 63 AND j < 63) THEN
                            rom_address <= STD_LOGIC_VECTOR(to_unsigned((i + 1) * 64 + j + 1, 12));
                        ELSE
                            check <= '1';
                        END IF;
                    END IF;

                    ram_address <= STD_LOGIC_VECTOR(to_unsigned(i * 64 + j, 12));
                    process_state <= calculate;

                ELSIF process_state = calculate THEN
                    IF (clock_cycle = 0) THEN --wait to set rom address
                        clock_cycle <= 1;
                    ELSIF (clock_cycle = 1) THEN --wait to set rom address
                        clock_cycle <= 2;
                    ELSE
                        write_enable_ram <= "0";
                        write_enable_mac <= '1';

                        IF (check = '1') THEN
                            input_1_mac <= (OTHERS => '0');
                            check <= '0';
                        ELSE
                            input_1_mac <= rom_data;
                        END IF;

                        input_2_mac <= filter_val;

                        IF filter_position = 8 THEN
                            IF min_max_done = '0' THEN
                                process_state <= cal_min_max;
                            ELSE
                                process_state <= write_ram;
                                write_enable_ram <= "1";
                            END IF;

                        ELSE
                            process_state <= read_rom; --search address
                            filter_position <= filter_position + 1;
                        END IF;
                        clock_cycle <= 0;

                    END IF;

                ELSIF process_state = cal_min_max THEN
                    IF (clock_cycle = 0) THEN
                        clock_cycle <= 1;
                    ELSIF (clock_cycle = 1) THEN
                        pix_val := to_integer(signed(data_out_mac));
                        IF (pix_val < min) THEN
                            min <= pix_val;
                        END IF;
                        IF (pix_val > max) THEN
                            max <= pix_val;
                        END IF;
                        clock_cycle <= 2;
                    ELSIF (clock_cycle = 2) THEN -- not used i think
                        clock_cycle <= 3;
                    ELSIF (clock_cycle = 3) THEN -- not used i think
                        clock_cycle <= 4;
                    ELSE
                        clock_cycle <= 0;
                        input_1_mac <= (OTHERS => '0');
                        input_2_mac <= (OTHERS => '0');
                        process_state <= go_next;
                    END IF;
                    filter_position <= 0;

                ELSIF process_state = write_ram THEN --  data is updated
                    --                    control_mac <= '1';
                    IF (clock_cycle = 0) THEN
                        clock_cycle <= 1;
                    ELSIF (clock_cycle = 1) THEN
                        pixel <= to_integer(signed(data_out_mac));
                        clock_cycle <= 2;
                    ELSIF (clock_cycle = 2) THEN
                        pixel <= pixel - min;
                        clock_cycle <= 3;
                    ELSIF (clock_cycle = 3) THEN
                        clock_cycle <= 4;
                    ELSIF (clock_cycle = 4) THEN
                        pixel <= 255 * pixel;
                        clock_cycle <= 5;
                    ELSIF (clock_cycle = 5) THEN
                        clock_cycle <= 6;
                    ELSIF (clock_cycle = 6) THEN
                        pixel <= pixel/diff;
                        clock_cycle <= 7;
                    ELSIF (clock_cycle = 7) THEN
                        clock_cycle <= 8;
                    ELSIF (clock_cycle = 8) THEN
                        clock_cycle <= 9;
                    ELSIF (clock_cycle = 9) THEN
                        clock_cycle <= 10;
                    ELSIF (clock_cycle = 10) THEN
                        clock_cycle <= 11;
                    ELSIF (clock_cycle = 11) THEN
                        clock_cycle <= 12;
                    ELSIF (clock_cycle = 12) THEN
                        ram_data_in <= STD_LOGIC_VECTOR(to_unsigned(pixel, 8));
                        clock_cycle <= 13;
                    ELSIF (clock_cycle = 13) THEN
                        clock_cycle <= 14;
                    ELSIF (clock_cycle = 14) THEN
                        clock_cycle <= 15;
                    ELSE
                        clock_cycle <= 0;
                        input_1_mac <= (OTHERS => '0');
                        input_2_mac <= (OTHERS => '0');
                        process_state <= go_next;
                    END IF;
                    filter_position <= 0;

                ELSIF process_state = go_next THEN -- next pixel is accessed or stop_reg<=1
                    write_enable_ram <= "0";
                    write_enable_mac <= '0';

                    IF (j = 63) THEN
                        IF (i = 63) THEN
                            IF min_max_done = '0' THEN
                                min_max_done <= '1';
                                diff <= max - min;
                                i <= 0;
                                j <= 0;
                            ELSE
                                stop_reg <= '1';
                            END IF;
                        ELSE
                            i <= i + 1;
                            j <= 0;
                        END IF;
                    ELSE
                        j <= j + 1;
                    END IF;
                    process_state <= read_rom;
                END IF;

            ELSIF (state = display) THEN
                IF (mode = '1') THEN
                    write_enable_ram <= (OTHERS => '0');
                    ram_address <= rdaddress;
                ELSE
                    rom_address <= rdaddress;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    PROCESS (ram_data, rom_data)
    BEGIN
        IF (state = display) THEN
            IF (mode = '1') THEN
                rgb_reg <= ram_data(7 DOWNTO 4) & ram_data(7 DOWNTO 4) & ram_data(7 DOWNTO 4);
            ELSE
                rgb_reg <= rom_data(7 DOWNTO 4) & rom_data(7 DOWNTO 4) & rom_data(7 DOWNTO 4);
            END IF;
        END IF;
    END PROCESS;
    PROCESS (video_on, newclk)
    BEGIN
        IF (state = display) THEN
            IF (rising_edge(newclk)) THEN
                IF video_on = '1' THEN
                    rgb <= rgb_reg;
                ELSE
                    rgb <= "000000000000";
                END IF;
            END IF;
        ELSE
            rgb <= "000000000000";
        END IF;
    END PROCESS;

END behaviour;