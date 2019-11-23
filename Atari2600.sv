//============================================================================
//  Atari 2600
// 
//  Port to MiSTer
//  Copyright (C) 2017,2018 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [45:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	output  [7:0] VIDEO_ARX,
	output  [7:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S, // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	input   [6:0] USER_IN,
	output  [6:0] USER_OUT,

	input         OSD_STATUS
);

assign ADC_BUS  = 'Z;
assign USER_OUT = '1;
assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = '0;
assign {SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 6'b111111;
assign SDRAM_DQ = 'Z;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;

assign LED_USER  = 0;
assign LED_DISK  = 0;
assign LED_POWER = 0;
assign BUTTONS   = 0;

assign VIDEO_ARX = status[8] ? 8'd16 : 8'd4;
assign VIDEO_ARY = status[8] ? 8'd9  : 8'd3; 

`include "build_id.v" 
localparam CONF_STR = {
	"ATARI2600;;",
	"F,*;",
	"O9A,SuperChip,Auto,Disable,Enable;",
	"-;",
	"O1,Colors,NTSC,PAL;",
	"O2,Video mode,Color,Mono;",
	"O8,Aspect ratio,4:3,16:9;", 
	"O57,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"-;",
	"O3,Difficulty P1,B,A;",
	"O4,Difficulty P2,B,A;",
	"-;",
	"OBC,Control,Joystick,Paddle,Auto(Single);",
	"ODE,Paddle map,X1+X2 X3+X4,X1+X3 X2+X4,X1+Y1 X2+Y2,X1-Y1 X2-Y2;",
	"OF,Paddle swap,No,Yes;",
	"OG,Swap Joysticks,No,Yes;",
	"OH,Serial Mode,None,SNAC;",
	"OI,Paddle ADC,No,Yes;",
	"R0,Reset;",
	"J1,Fire,Paddle1(x),Paddle2(y),Game Reset,Game Select;",
	"V,v",`BUILD_DATE
};

////////////////////   CLOCKS   ///////////////////

wire locked;
wire clk_sys,clk_cpu;
wire clk_mem;

pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_sys),
	.outclk_1(clk_cpu),
	.outclk_2(CLK_VIDEO),
	.locked(locked)
);

//assign CLK_VIDEO = clk_sys;

reg ce_pix;
always @(negedge CLK_VIDEO) begin
	reg [4:0] div;

	div <= div + 1'd1;
	if(div == 23) div <= 0;
	ce_pix <= !div;
end

wire reset = RESET | status[0] | buttons[1] | ioctl_download;


//////////////////   HPS I/O   ///////////////////
wire [15:0] joy_0,joy_1,joy_2,joy_3;
wire [15:0] joya_0,joya_1,joya_2,joya_3;
wire  [1:0] buttons;
wire [31:0] status;
wire [24:0] ps2_mouse;

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index; 
wire [31:0] ioctl_file_ext;

wire        forced_scandoubler;

hps_io #(.STRLEN($size(CONF_STR)>>3)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.conf_str(CONF_STR),

	.joystick_0(joy_0),
	.joystick_1(joy_1),
	.joystick_2(joy_2),
	.joystick_3(joy_3),
	.joystick_analog_0(joya_0),
	.joystick_analog_1(joya_1),
	.joystick_analog_2(joya_2),
	.joystick_analog_3(joya_3),

	.buttons(buttons),
	.status(status),
	.forced_scandoubler(forced_scandoubler),

	.ps2_kbd_led_use(0),
	.ps2_kbd_led_status(0),

	.ps2_mouse(ps2_mouse),

	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_file_ext(ioctl_file_ext),

	.sd_lba(0),
	.sd_rd(0),
	.sd_wr(0),
	.sd_conf(0),
	.sd_buff_din(0),
	.ioctl_wait(0)
);

wire [14:0] rom_addr;
wire  [7:0] rom_data;

dpram #(15, 8, "rom.mif") rom
(
	.clock(clk_sys),

	.data_a(ioctl_dout),
	.address_a(ioctl_addr[15:0]),
	.wren_a(ioctl_wr),

	.address_b(rom_addr),
	.q_b(rom_data)
);

wire [23:0] ext = (ioctl_file_ext[23:16] == ".") ? ioctl_file_ext[23:0] : ioctl_file_ext[31:8];

reg [3:0] force_bs = 0;
reg sc = 0;
always @(posedge clk_sys) begin
	reg       old_download;

	old_download <= ioctl_download;
	if(~old_download & ioctl_download) begin
		force_bs <= 0;
		sc <= status[9];
		if (ext == ".F8") force_bs <= 1;
		if (ext == ".F6") force_bs <= 2;
		if (ext == ".FE") force_bs <= 3;
		if (ext == ".E0") force_bs <= 4;
		if (ext == ".3F") force_bs <= 5;
    	if (ext == ".F4") force_bs <= 6;
		if (ext == ".P2") force_bs <= 7; // Pitfall II
    	if (ext == ".FA") force_bs <= 8;
    	if (ext == ".CV") force_bs <= 9;
	
		sc <= (!status[10:9]) ? (ioctl_file_ext[8:0] == "S") : status[10];
	end
end

wire [4:0] audio;
assign AUDIO_R = {3{audio}};
assign AUDIO_L = AUDIO_R;
assign AUDIO_S = 0;
assign AUDIO_MIX = 0;

wire joy_swap = status[16];
wire serial = status[17];
wire ADC = status[18];

wire p_1 = ADC ? USER_IN[5] : (status[14] ? ~j0[5] : ~|j0[6:5]);
wire p_2 = ADC ? USER_IN[3] : (status[14] ? ~j0[6] : status[13] ? ~|joy_2[6:5] : ~|joy_1[6:5]);
wire p_3 = status[14] ? ~joy_1[5] : status[13] ? ~|joy_1[6:5] : ~|joy_2[6:5];
wire p_4 = status[14] ? ~joy_1[6] : ~|joy_3[6:5];

wire [7:0] paddle_1 = ax;
wire [7:0] paddle_2 = status[14] ? (status[13] ? ~ay : ay) : status[13] ? joya_2[7:0] : joya_1[7:0];
wire [7:0] paddle_3 = status[14] ? joya_1[7:0] : status[13] ? joya_1[7:0] : joya_2[7:0];
wire [7:0] paddle_4 = status[14] ? (status[13] ? ~joya_1[15:8] : joya_1[15:8]) : joya_3[7:0];

A2601top A2601top
(
	.reset(reset),
	.clk(clk_cpu),

	.audio(audio),

	//.O_VSYNC(VSync),
	.O_HSYNC(hs),
	.O_HBLANK(HBlank),
	.O_VBLANK(VBlank),
	.O_VIDEO_R(R),
	.O_VIDEO_G(G),
	.O_VIDEO_B(B),

	.p1_r(joy_swap ? (serial ? ~joy_0[0] : ~joy_1[0]) : (serial ? USER_IN[3] : ~joy_0[0])),	
	.p1_l(joy_swap ? (serial ? ~joy_0[1] : ~joy_1[1]) : (serial ? USER_IN[5] : ~joy_0[1])),
	.p1_d(joy_swap ? (serial ? ~joy_0[2] : ~joy_1[2]) : (serial ? USER_IN[0] : ~joy_0[2])),
	.p1_u(joy_swap ? (serial ? ~joy_0[3] : ~joy_1[3]) : (serial ? USER_IN[1] : ~joy_0[3])),
	.p1_f(joy_swap ? (serial ? ~joy_0[4] : ~joy_1[4]) : (serial ? USER_IN[2] : ~joy_0[4])),

	.p2_r(joy_swap ? (serial ? USER_IN[3] : ~joy_0[0]) : (serial ? ~joy_0[0] : ~joy_1[0])),
	.p2_l(joy_swap ? (serial ? USER_IN[5] : ~joy_0[1]) : (serial ? ~joy_0[1] : ~joy_1[1])),
	.p2_d(joy_swap ? (serial ? USER_IN[0] : ~joy_0[2]) : (serial ? ~joy_0[2] : ~joy_1[2])),
	.p2_u(joy_swap ? (serial ? USER_IN[1] : ~joy_0[3]) : (serial ? ~joy_0[3] : ~joy_1[3])),
	.p2_f(joy_swap ? (serial ? USER_IN[2] : ~joy_0[4]) : (serial ? ~joy_0[4] : ~joy_1[4])),

	.p_1(status[15] ? p_2 : p_1),
	.p_2(status[15] ? p_1 : p_2),
	.p_3(status[15] ? p_4 : p_3),
	.p_4(status[15] ? p_3 : p_4),

	.paddle_1(status[15] ? paddle_2 : paddle_1),
	.paddle_2(status[15] ? paddle_1 : paddle_2),
	.paddle_3(status[15] ? paddle_4 : paddle_3),
	.paddle_4(status[15] ? paddle_3 : paddle_4),

	.p_start (~(j0[7] | joy_1[7] | joy_2[7] | joy_3[7])),
	.p_select(~(j0[8] | joy_1[8] | joy_2[8] | joy_3[8])),
	
	.p_type(status[12:11]),

	.p_color(~status[2]),

	.sc(sc),
	.force_bs(force_bs),
	.rom_size(ioctl_addr[16:0]),
	.rom_a(rom_addr),
	.rom_do(rom_data),

	.pal(status[1]),
	.p_dif(status[4:3])
);

wire [7:0] R,G,B;
wire hs;
reg  HSync;
wire HBlank, VBlank;
reg VSync;

always @(posedge CLK_VIDEO) begin
	reg       old_vbl;
	reg [2:0] vbl;
	reg [7:0] vblcnt, vspos;
	
	HSync <= hs;
	if(~HSync & hs) begin
		old_vbl <= VBlank;
		
		if(VBlank) vblcnt <= vblcnt+1'd1;
		if(~old_vbl & VBlank) vblcnt <= 0;
		if(old_vbl & ~VBlank) vspos <= (vblcnt>>1) - 8'd10;

		{VSync,vbl} <= {vbl,1'b0};
		if(vblcnt == vspos) {VSync,vbl} <= '1;
	end
end

wire [2:0] scale = status[7:5];
wire [2:0] sl = scale ? scale - 1'd1 : 3'd0;
wire       scandoubler = scale || forced_scandoubler;

assign VGA_F1 = 0;
assign VGA_SL = sl[1:0];
assign VGA_DE = de & ~(VGA_VS|VGA_HS);

wire de;

video_mixer #(.LINE_LENGTH(250)) video_mixer
(
	.*,
	.clk_sys(CLK_VIDEO),
	.ce_pix(ce_pix),
	.ce_pix_out(CE_PIXEL),

	.scanlines(0),
	.hq2x(scale==1),
	.mono(0),

	.VGA_DE(de)
);

//////////////////   ANALOG AXIS   ///////////////////
reg        emu = 0;
wire [7:0] ax = ADC ? serx : (emu ? mx[7:0] : joya_0[7:0]);
wire [7:0] ay = ADC ? sery : (emu ? my[7:0] : joya_0[15:8]);
wire [8:0] j0 = emu ? {1'b0, ps2_mouse[2:0], joy_0[4:0]} : joy_0[8:0];

//use LUT to to do the math for paddles
wire signed [7:0] serx = paddle_lut[dout[11:0]] - 128;
wire signed [7:0] sery = paddle_lut[dout[23:12]] - 128;

reg  signed [8:0] mx = 0;
wire signed [8:0] mdx = {ps2_mouse[4],ps2_mouse[4],ps2_mouse[15:9]};
wire signed [8:0] mdx2 = (mdx > 10) ? 9'd10 : (mdx < -10) ? -8'd10 : mdx;
wire signed [8:0] nmx = mx + mdx2;

reg  signed [8:0] my = 0;
wire signed [8:0] mdy = {ps2_mouse[5],ps2_mouse[5],ps2_mouse[23:17]};
wire signed [8:0] mdy2 = (mdy > 10) ? 9'd10 : (mdy < -10) ? -9'd10 : mdy;
wire signed [8:0] nmy = my + mdy2;

always @(posedge clk_sys) begin
	reg old_stb = 0;
	
	old_stb <= ps2_mouse[24];
	if(old_stb != ps2_mouse[24]) begin
		emu <= 1;
		mx <= (nmx < -128) ? -9'd128 : (nmx > 127) ? 9'd127 : nmx;
		my <= (nmy < -128) ? -9'd128 : (nmy > 127) ? 9'd127 : nmy;
	end

	if(joya_0) begin
		emu <= 0;
		mx <= 0;
		my <= 0;
	end
end

//adc
wire tape_sync;
//reg   [23:0] dout2;
reg   [23:0] dout;
ltc2308 ltc2308
(
	.clk(CLK_50M),
	.ADC_BUS(ADC_BUS),
	.dout_sync(tape_sync),
	.dout(dout)
);

reg signed [11:0] paddle_lut [4096] = '{'d0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd0, 'd1, 'd1, 'd1, 'd1, 'd1, 'd2, 'd2, 'd2, 'd2, 'd2, 'd3, 'd3, 'd3, 'd3, 'd4, 'd4, 'd4, 'd4, 'd4, 'd5, 'd5, 'd5, 'd5, 'd5, 'd6, 'd6, 'd6, 'd6, 'd7, 'd7, 'd7, 'd7, 'd7, 'd8, 'd8, 'd8, 'd8, 'd8, 'd9, 'd9, 'd9, 'd9, 'd9, 'd10, 'd10, 'd10, 'd10, 'd11, 'd11, 'd11, 'd11, 'd11, 'd12, 'd12, 'd12, 'd12, 'd12, 'd13, 'd13, 'd13, 'd13, 'd14, 'd14, 'd14, 'd14, 'd14, 'd15, 'd15, 'd15, 'd15, 'd15, 'd16, 'd16, 'd16, 'd16, 'd17, 'd17, 'd17, 'd17, 'd17, 'd18, 'd18, 'd18, 'd18, 'd18, 'd19, 'd19, 'd19, 'd19, 'd19, 'd20, 'd20, 'd20, 'd20, 'd21, 'd21, 'd21, 'd21, 'd21, 'd22, 'd22, 'd22, 'd22, 'd22, 'd23, 'd23, 'd23, 'd23, 'd24, 'd24, 'd24, 'd24, 'd24, 'd25, 'd25, 'd25, 'd25, 'd25, 'd26, 'd26, 'd26, 'd26, 'd26, 'd27, 'd27, 'd27, 'd27, 'd28, 'd28, 'd28, 'd28, 'd28, 'd29, 'd29, 'd29, 'd29, 'd29, 'd30, 'd30, 'd30, 'd30, 'd31, 'd31, 'd31, 'd31, 'd31, 'd32, 'd32, 'd32, 'd32, 'd32, 'd33, 'd33, 'd33, 'd33, 'd34, 'd34, 'd34, 'd34, 'd34, 'd35, 'd35, 'd35, 'd35, 'd35, 'd36, 'd36, 'd36, 'd36, 'd36, 'd37, 'd37, 'd37, 'd37, 'd38, 'd38, 'd38, 'd38, 'd38, 'd39, 'd39, 'd39, 'd39, 'd39, 'd40, 'd40, 'd40, 'd40, 'd41, 'd41, 'd41, 'd41, 'd41, 'd42, 'd42, 'd42, 'd42, 'd42, 'd43, 'd43, 'd43, 'd43, 'd43, 'd44, 'd44, 'd44, 'd44, 'd45, 'd45, 'd45, 'd45, 'd45, 'd46, 'd46, 'd46, 'd46, 'd46, 'd47, 'd47, 'd47, 'd47, 'd48, 'd48, 'd48, 'd48, 'd48, 'd49, 'd49, 'd49, 'd49, 'd49, 'd50, 'd50, 'd50, 'd50, 'd51, 'd51, 'd51, 'd51, 'd51, 'd52, 'd52, 'd52, 'd52, 'd52, 'd53, 'd53, 'd53, 'd53, 'd53, 'd54, 'd54, 'd54, 'd54, 'd55, 'd55, 'd55, 'd55, 'd55, 'd56, 'd56, 'd56, 'd56, 'd56, 'd57, 'd57, 'd57, 'd57, 'd58, 'd58, 'd58, 'd58, 'd58, 'd59, 'd59, 'd59, 'd59, 'd59, 'd60, 'd60, 'd60, 'd60, 'd60, 'd61, 'd61, 'd61, 'd61, 'd62, 'd62, 'd62, 'd62, 'd62, 'd63, 'd63, 'd63, 'd63, 'd63, 'd64, 'd64, 'd64, 'd64, 'd65, 'd65, 'd65, 'd65, 'd65, 'd66, 'd66, 'd66, 'd66, 'd66, 'd67, 'd67, 'd67, 'd67, 'd68, 'd68, 'd68, 'd68, 'd68, 'd69, 'd69, 'd69, 'd69, 'd69, 'd70, 'd70, 'd70, 'd70, 'd70, 'd71, 'd71, 'd71, 'd71, 'd72, 'd72, 'd72, 'd72, 'd72, 'd73, 'd73, 'd73, 'd73, 'd73, 'd74, 'd74, 'd74, 'd74, 'd75, 'd75, 'd75, 'd75, 'd75, 'd76, 'd76, 'd76, 'd76, 'd76, 'd77, 'd77, 'd77, 'd77, 'd77, 'd78, 'd78, 'd78, 'd78, 'd79, 'd79, 'd79, 'd79, 'd79, 'd80, 'd80, 'd80, 'd80, 'd80, 'd81, 'd81, 'd81, 'd81, 'd82, 'd82, 'd82, 'd82, 'd82, 'd83, 'd83, 'd83, 'd83, 'd83, 'd84, 'd84, 'd84, 'd84, 'd85, 'd85, 'd85, 'd85, 'd85, 'd86, 'd86, 'd86, 'd86, 'd86, 'd87, 'd87, 'd87, 'd87, 'd87, 'd88, 'd88, 'd88, 'd88, 'd89, 'd89, 'd89, 'd89, 'd89, 'd90, 'd90, 'd90, 'd90, 'd90, 'd91, 'd91, 'd91, 'd91, 'd92, 'd92, 'd92, 'd92, 'd92, 'd93, 'd93, 'd93, 'd93, 'd93, 'd94, 'd94, 'd94, 'd94, 'd94, 'd95, 'd95, 'd95, 'd95, 'd96, 'd96, 'd96, 'd96, 'd96, 'd97, 'd97, 'd97, 'd97, 'd97, 'd98, 'd98, 'd98, 'd98, 'd99, 'd99, 'd99, 'd99, 'd99, 'd100, 'd100, 'd100, 'd100, 'd100, 'd101, 'd101, 'd101, 'd101, 'd102, 'd102, 'd102, 'd102, 'd102, 'd103, 'd103, 'd103, 'd103, 'd103, 'd104, 'd104, 'd104, 'd104, 'd104, 'd105, 'd105, 'd105, 'd105, 'd106, 'd106, 'd106, 'd106, 'd106, 'd107, 'd107, 'd107, 'd107, 'd107, 'd108, 'd108, 'd108, 'd108, 'd109, 'd109, 'd109, 'd109, 'd109, 'd110, 'd110, 'd110, 'd110, 'd110, 'd111, 'd111, 'd111, 'd111, 'd111, 'd112, 'd112, 'd112, 'd112, 'd113, 'd113, 'd113, 'd113, 'd113, 'd114, 'd114, 'd114, 'd114, 'd114, 'd115, 'd115, 'd115, 'd115, 'd116, 'd116, 'd116, 'd116, 'd116, 'd117, 'd117, 'd117, 'd117, 'd117, 'd118, 'd118, 'd118, 'd118, 'd119, 'd119, 'd119, 'd119, 'd119, 'd120, 'd120, 'd120, 'd120, 'd120, 'd121, 'd121, 'd121, 'd121, 'd121, 'd122, 'd122, 'd122, 'd122, 'd123, 'd123, 'd123, 'd123, 'd123, 'd124, 'd124, 'd124, 'd124, 'd124, 'd125, 'd125, 'd125, 'd125, 'd126, 'd126, 'd126, 'd126, 'd126, 'd127, 'd127, 'd127, 'd127, 'd127, 'd128, 'd128, 'd128, 'd128, 'd128, 'd129, 'd129, 'd129, 'd129, 'd130, 'd130, 'd130, 'd130, 'd130, 'd131, 'd131, 'd131, 'd131, 'd131, 'd132, 'd132, 'd132, 'd132, 'd133, 'd133, 'd133, 'd133, 'd133, 'd134, 'd134, 'd134, 'd134, 'd134, 'd135, 'd135, 'd135, 'd135, 'd136, 'd136, 'd136, 'd136, 'd136, 'd137, 'd137, 'd137, 'd137, 'd137, 'd138, 'd138, 'd138, 'd138, 'd138, 'd139, 'd139, 'd139, 'd139, 'd140, 'd140, 'd140, 'd140, 'd140, 'd141, 'd141, 'd141, 'd141, 'd141, 'd142, 'd142, 'd142, 'd142, 'd143, 'd143, 'd143, 'd143, 'd143, 'd144, 'd144, 'd144, 'd144, 'd144, 'd145, 'd145, 'd145, 'd145, 'd145, 'd146, 'd146, 'd146, 'd146, 'd147, 'd147, 'd147, 'd147, 'd147, 'd148, 'd148, 'd148, 'd148, 'd148, 'd149, 'd149, 'd149, 'd149, 'd150, 'd150, 'd150, 'd150, 'd150, 'd151, 'd151, 'd151, 'd151, 'd151, 'd152, 'd152, 'd152, 'd152, 'd153, 'd153, 'd153, 'd153, 'd153, 'd154, 'd154, 'd154, 'd154, 'd154, 'd155, 'd155, 'd155, 'd155, 'd155, 'd156, 'd156, 'd156, 'd156, 'd157, 'd157, 'd157, 'd157, 'd157, 'd158, 'd158, 'd158, 'd158, 'd158, 'd159, 'd159, 'd159, 'd159, 'd160, 'd160, 'd160, 'd160, 'd160, 'd161, 'd161, 'd161, 'd161, 'd161, 'd162, 'd162, 'd162, 'd162, 'd162, 'd163, 'd163, 'd163, 'd163, 'd164, 'd164, 'd164, 'd164, 'd164, 'd165, 'd165, 'd165, 'd165, 'd165, 'd166, 'd166, 'd166, 'd166, 'd167, 'd167, 'd167, 'd167, 'd167, 'd168, 'd168, 'd168, 'd168, 'd168, 'd169, 'd169, 'd169, 'd169, 'd170, 'd170, 'd170, 'd170, 'd170, 'd171, 'd171, 'd171, 'd171, 'd171, 'd172, 'd172, 'd172, 'd172, 'd172, 'd173, 'd173, 'd173, 'd173, 'd174, 'd174, 'd174, 'd174, 'd174, 'd175, 'd175, 'd175, 'd175, 'd175, 'd176, 'd176, 'd176, 'd176, 'd177, 'd177, 'd177, 'd177, 'd177, 'd178, 'd178, 'd178, 'd178, 'd178, 'd179, 'd179, 'd179, 'd179, 'd179, 'd180, 'd180, 'd180, 'd180, 'd181, 'd181, 'd181, 'd181, 'd181, 'd182, 'd182, 'd182, 'd182, 'd182, 'd183, 'd183, 'd183, 'd183, 'd184, 'd184, 'd184, 'd184, 'd184, 'd185, 'd185, 'd185, 'd185, 'd185, 'd186, 'd186, 'd186, 'd186, 'd187, 'd187, 'd187, 'd187, 'd187, 'd188, 'd188, 'd188, 'd188, 'd188, 'd189, 'd189, 'd189, 'd189, 'd189, 'd190, 'd190, 'd190, 'd190, 'd191, 'd191, 'd191, 'd191, 'd191, 'd192, 'd192, 'd192, 'd192, 'd192, 'd193, 'd193, 'd193, 'd193, 'd194, 'd194, 'd194, 'd194, 'd194, 'd195, 'd195, 'd195, 'd195, 'd195, 'd196, 'd196, 'd196, 'd196, 'd196, 'd197, 'd197, 'd197, 'd197, 'd198, 'd198, 'd198, 'd198, 'd198, 'd199, 'd199, 'd199, 'd199, 'd199, 'd200, 'd200, 'd200, 'd200, 'd201, 'd201, 'd201, 'd201, 'd201, 'd202, 'd202, 'd202, 'd202, 'd202, 'd203, 'd203, 'd203, 'd203, 'd204, 'd204, 'd204, 'd204, 'd204, 'd205, 'd205, 'd205, 'd205, 'd205, 'd206, 'd206, 'd206, 'd206, 'd206, 'd207, 'd207, 'd207, 'd207, 'd208, 'd208, 'd208, 'd208, 'd208, 'd209, 'd209, 'd209, 'd209, 'd209, 'd210, 'd210, 'd210, 'd210, 'd211, 'd211, 'd211, 'd211, 'd211, 'd212, 'd212, 'd212, 'd212, 'd212, 'd213, 'd213, 'd213, 'd213, 'd213, 'd214, 'd214, 'd214, 'd214, 'd215, 'd215, 'd215, 'd215, 'd215, 'd216, 'd216, 'd216, 'd216, 'd216, 'd217, 'd217, 'd217, 'd217, 'd218, 'd218, 'd218, 'd218, 'd218, 'd219, 'd219, 'd219, 'd219, 'd219, 'd220, 'd220, 'd220, 'd220, 'd221, 'd221, 'd221, 'd221, 'd221, 'd222, 'd222, 'd222, 'd222, 'd222, 'd223, 'd223, 'd223, 'd223, 'd223, 'd224, 'd224, 'd224, 'd224, 'd225, 'd225, 'd225, 'd225, 'd225, 'd226, 'd226, 'd226, 'd226, 'd226, 'd227, 'd227, 'd227, 'd227, 'd228, 'd228, 'd228, 'd228, 'd228, 'd229, 'd229, 'd229, 'd229, 'd229, 'd230, 'd230, 'd230, 'd230, 'd230, 'd231, 'd231, 'd231, 'd231, 'd232, 'd232, 'd232, 'd232, 'd232, 'd233, 'd233, 'd233, 'd233, 'd233, 'd234, 'd234, 'd234, 'd234, 'd235, 'd235, 'd235, 'd235, 'd235, 'd236, 'd236, 'd236, 'd236, 'd236, 'd237, 'd237, 'd237, 'd237, 'd238, 'd238, 'd238, 'd238, 'd238, 'd239, 'd239, 'd239, 'd239, 'd239, 'd240, 'd240, 'd240, 'd240, 'd240, 'd241, 'd241, 'd241, 'd241, 'd242, 'd242, 'd242, 'd242, 'd242, 'd243, 'd243, 'd243, 'd243, 'd243, 'd244, 'd244, 'd244, 'd244, 'd245, 'd245, 'd245, 'd245, 'd245, 'd246, 'd246, 'd246, 'd246, 'd246, 'd247, 'd247, 'd247, 'd247, 'd247, 'd248, 'd248, 'd248, 'd248, 'd249, 'd249, 'd249, 'd249, 'd249, 'd250, 'd250, 'd250, 'd250, 'd250, 'd251, 'd251, 'd251, 'd251, 'd252, 'd252, 'd252, 'd252, 'd252, 'd253, 'd253, 'd253, 'd253, 'd253, 'd254, 'd254, 'd254, 'd254, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255, 'd255};

endmodule
