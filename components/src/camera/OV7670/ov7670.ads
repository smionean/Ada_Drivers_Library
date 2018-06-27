------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL;     use HAL;
with HAL.I2C; use HAL.I2C;
with OV2640;

package OV7670 is

   OV7670_PID : constant := 16#76#;

   type Pixel_Format is (Pix_RGB565, Pix_RGB555, Pix_RGB444);

   type OV7670_Camera (I2C : not null Any_I2C_Port) is private;

   procedure Initialize (This : in out OV7670_Camera;
                         Addr : I2C_Address);

   procedure Set_Pixel_Format (This : OV7670_Camera;
                               Pix  : Pixel_Format);

   procedure Set_Frame_Size (This : OV7670_Camera;
                             Res  : OV2640.Frame_Size);

   function Get_PID (This : OV7670_Camera) return UInt8;

private

   type OV7670_Camera (I2C  : not null Any_I2C_Port) is record
      Addr : UInt10;
   end record;

   REG_GAIN     : constant := 16#00#; --/*Gain lower 8 bits (rest in vref) */
   REG_BLUE     : constant := 16#01#; --/*blue gain */
   REG_RED      : constant := 16#02#; --/*red gain */
   REG_VREF     : constant := 16#03#; --/*Pieces of GAIN, VSTART, VSTOP */
   REG_COM1     : constant := 16#04#; --/*Control 1 */
   REG_BAVE     : constant := 16#05#; --/*U/B Average level */
   REG_GbAVE    : constant := 16#06#; -- /*Y/Gb Average level */
   REG_AECHH    : constant := 16#07#; -- /*AEC MS 5 bits */
   REG_RAVE     : constant := 16#08#; -- /*V/R Average level */
   REG_COM2     : constant := 16#09#; -- /*Control 2 */
   REG_PID      : constant := 16#0A#; --  /*Product ID MSB */
   REG_VER      : constant := 16#0B#; --  /*Product ID LSB */
   REG_COM3     : constant := 16#0C#; -- /*Control 3 */
   REG_COM4     : constant := 16#0d#; -- /*Control 4 */
   REG_COM5     : constant := 16#0E#; -- /*All "reserved" */
   REG_COM6     : constant := 16#0F#; -- /*Control 6 */
   REG_AECH     : constant := 16#10#; -- /*More bits of AEC value */
   REG_CLKRC    : constant := 16#11#; -- /*Clocl control */
   REG_COM7     : constant := 16#12#; -- /*Control 7 */
   REG_COM8     : constant := 16#13#; -- /*Control 8 */
   REG_COM9     : constant := 16#14#; -- /*Control 9  - gain ceiling */
   REG_COM10    : constant := 16#15#; -- /*Control 10 */
   --  16#16#; -- reserved
   REG_HSTART   : constant := 16#17#; -- /*Horiz start high bits */
   REG_HSTOP    : constant := 16#18#; -- /*Horiz stop high bits */
   REG_VSTART   : constant := 16#19#; -- /*Vert start high bits */
   REG_VSTOP    : constant := 16#1A#; -- /*Vert stop high bits */
   REG_PSHFT    : constant := 16#1B#; -- /*Pixel delay after HREF */
   REG_MIDH     : constant := 16#1C#; -- /*Manuf. ID high */
   REG_MIDL     : constant := 16#1D#; -- /*Manuf. ID low */
   REG_MVFP     : constant := 16#1E#; -- /*Mirror / vflip */
   REG_LAEC     : constant := 16#1F#; -- reserved
   REG_ADCCTR0  : constant := 16#20#; -- ADC control
   REG_ADCCTR1  : constant := 16#21#; -- reserved
   REG_ADCCTR2  : constant := 16#22#; -- reserved
   REG_ADCCTR4  : constant := 16#23#; -- reserved
   REG_AEW      : constant := 16#24#; -- /*AGC upper limit */
   REG_AEB      : constant := 16#25#; -- /*AGC lower limit */
   REG_VPT      : constant := 16#26#; -- /*AGC/AEC fast mode op region */
   REG_BBIAS    : constant := 16#27#; -- B channel signal output bias
   REG_GbBIAS   : constant := 16#28#; -- Gb Channel output bias
   --  16#29#; -- reserved
   REG_EXHCH    : constant := 16#2A#; -- Dummy pixel insert MSB
   REG_EXHCL    : constant := 16#2B#; -- Dummy pixel insert LSB
   REG_RBIAS    : constant := 16#2C#; -- R Channel signal output bias
   REG_ADVFL    : constant := 16#2D#; -- LSB if insert dummy lines in vertical direction
   REG_ADVFH    : constant := 16#2E#; -- MSB of insert dummy lines in vertical direction
   REG_YAVE     : constant := 16#2F#; -- Y/G Channel average value
   REG_HSYST    : constant := 16#30#; -- HSYNC rising edge delay
   REG_HSYEN    : constant := 16#31#; -- /*HSYNC falling edge delay */
   REG_HREF     : constant := 16#32#; -- /*HREF pieces */
   REG_CHLF     : constant := 16#33#; -- array current control
   REG_ARBLM    : constant := 16#34#; -- array reference control
                                      -- 16#35#; -- reserved
                                      -- 16#36#; -- reserved
   REG_ADC      : constant := 16#37#; -- ADC control
   REG_ACOM     : constant := 16#38#; -- ADC and analog cmmon mode control
   REG_OFON     : constant := 16#39#; -- ADC offset control
   REG_TSLB     : constant := 16#3A#; -- line buffer test option
   REG_COM11    : constant := 16#3B#; -- /*Control 11 */
   REG_COM12    : constant := 16#3C#; -- /*Control 12 */
   REG_COM13    : constant := 16#3D#; -- /*Control 13 */
   REG_COM14    : constant := 16#3E#; -- /*Control 14 */
   REG_EDGE     : constant := 16#3F#; -- /*Edge enhancement factor */
   REG_COM15    : constant := 16#40#; -- /*Control 15 */
   REG_COM16    : constant := 16#41#; -- /*Control 16 */
   REG_COM17    : constant := 16#42#; -- /*Control 17 */
   REG_AWBC1    : constant := 16#43#; -- reserved
   REG_AWBC2    : constant := 16#44#; -- reserved
   REG_AWBC3    : constant := 16#45#; -- reserved
   REG_AWBC4    : constant := 16#46#; -- reserved
   REG_AWBC5    : constant := 16#47#; -- reserved
   REG_AWBC6    : constant := 16#48#; -- reserved
                                      -- 16#49#; -- reserved
                                      -- 16#4A#; -- reserved
   REG_REG4B    : constant := 16#4B#; -- regiter 4B
   REG_DNSTH    : constant := 16#4C#; -- De-noise threshold
                                      -- 16#4D#; -- reserved
                                      -- 16#4E#; -- reserved
   REG_MTX1     : constant := 16#4F#; -- /*Matrix Coefficient 1 */
   REG_MTX2     : constant := 16#50#; -- /*Matrix Coefficient 2 */
   REG_MTX3     : constant := 16#51#; -- /*Matrix Coefficient 3 */
   REG_MTX4     : constant := 16#52#; -- /*Matrix Coefficient 4 */
   REG_MTX5     : constant := 16#53#; -- /*Matrix Coefficient 5 */
   REG_MTX6     : constant := 16#54#; -- /*Matrix Coefficient 6 */
   REG_BRIGHT   : constant := 16#55#; -- brightness control
   REG_CONTRAS  : constant := 16#56#; -- /*Contrast control */
   REG_CONTRASCENTER : constant := 16#57#; -- contrast center
   REG_MTXS     : constant := 16#58#; -- matrix coefficient sign for coefficient 5 to 0
                                      -- 16#59# --  reserved : AWB Conntrol
                                      -- 5A to 60 --
                                      -- 16#61# --  reserved : AWB Conntrol
   REG_LCC1     : constant := 16#62#; -- lens correction option 1
   REG_LCC2     : constant := 16#63#; -- lens correction option 2
   REG_LCC3     : constant := 16#64#; -- lens correction option 3
   REG_LCC4     : constant := 16#65#; -- lens correction option 4
   REG_LCC5     : constant := 16#66#; -- lens correction control
   REG_MANU     : constant := 16#67#; -- manual U value
   REG_MANV     : constant := 16#68#; -- manual V value
   REG_GFIX     : constant := 16#69#; -- /*Fix gain control */
   REG_GGAIN    : constant := 16#6A#; -- /*G Channel AWB Gain */
   REG_DBLV     : constant := 16#6B#; -- PLL control; regulator control; clock divider control for DSP scale
   REG_AWBCTR3  : constant := 16#6C#; -- /*AWB Control 3 */
   REG_AWBCTR2  : constant := 16#6D#; -- /*AWB Control 2 */
   REG_AWBCTR1  : constant := 16#6E#; -- /*AWB Control 1 */
   REG_AWBCTR0  : constant := 16#6F#; -- /*AWB Control 0 */
   REG_SCALING_XSC : constant := 16#70#; --
   REG_SCALING_YSC : constant := 16#71#; --
   REG_SCALING_DCWCTR : constant := 16#72#; -- DCW Control
   REG_SCALING_PCLK_DIV : constant := 16#73#; --
   REG_REG74    : constant := 16#74#; --
   REG_REG75    : constant := 16#75#; --
   REG_REG76    : constant := 16#76#; --
   REG_REG77    : constant := 16#77#; --
                                      -- 16#78# -- reserved
                                      -- 16#79# -- reserved
   REG_GAM1     : constant := 16#7A#; -- gamma curve 1st segment input endpoint 16#010# output value
   REG_GAM2     : constant := 16#7B#; -- gamma curve 2nd segment input endpoint 16#020# output value
   REG_GAM3     : constant := 16#7C#; -- gamma curve 3rd segment input endpoint 16#040# output value
   REG_GAM4     : constant := 16#7D#; -- gamma curve 4th segment input endpoint 16#0A0# output value
   REG_GAM5     : constant := 16#7E#; -- gamma curve 5th segment input endpoint 16#0C0# output value
   REG_GAM6     : constant := 16#7F#; -- gamma curve 6th segment input endpoint 16#0E0# output value
   REG_GAM7     : constant := 16#80#; -- gamma curve 7th segment input endpoint 16#100# output value
   REG_GAM8     : constant := 16#81#; -- gamma curve 8th segment input endpoint 16#120# output value
   REG_GAM9     : constant := 16#82#; -- gamma curve 9th segment input endpoint 16#140# output value
   REG_GAM10    : constant := 16#83#; -- gamma curve 10th segment input endpoint 16#180# output value
   REG_GAM11    : constant := 16#84#; -- gamma curve 11th segment input endpoint 16#1C0# output value
   REG_GAM12    : constant := 16#85#; -- gamma curve 12th segment input endpoint 16#240# output value
   REG_GAM13    : constant := 16#86#; -- gamma curve 13th segment input endpoint 16#2C0# output value
   REG_GAM14    : constant := 16#87#; -- gamma curve 14th segment input endpoint 16#340# output value
   --  16#8A# to 16#91# reserved
   REG_DM_LNL   : constant := 16#92#; -- dummy line low 8 bits
   REG_DM_LNH   : constant := 16#93#; -- dummy line high 8 bits
   REG_LCC6     : constant := 16#94#; -- lens correction option 6
   REG_LCC7     : constant := 16#95#; -- lens correction option 7
   --  16#96# to 16#9C#; -- reserved
   REG_BD50ST   : constant := 16#9D#; -- 50Hz banding filter value
   REG_BD60ST   : constant := 16#9E#; -- 60Hz banding filter value
   --  16#9F# to 16#AB reserved : histogram-based AEC/AGC cntrol

   REG_STR_OPT  : constant := 16#AC#; -- Register AC
                                      -- bit[7] : strobe enable
                                      -- bit[6] : R/G/B gain controlled by STR_R (0xAD) / STR_G
                                      -- bit[5:4] : xenon mode option
                                      --          00 : 1 line
                                      --          01 : 2 lines
                                      --          10 : 3 lines
                                      --          11 : 4 lines
                                      -- bit[3:2] : reserved
                                      -- bit[1:0] : mode select
                                      --          00: Xenon
                                      --          01: LED 1&2
                                      --          1x: LED 3

   REG_STR_R    : constant := 16#AD#; -- gain for led output frame R
   REG_STR_G    : constant := 16#AE#; -- gain for led output frame G
   REG_STR_B    : constant := 16#AF#; -- gain for led output frame B
   --  16#B0# to 16#B2# -- reserved
   REG_THL_ST   : constant := 16#B3#; -- digital BLC Target
   --  16#B5# -- reserved
   REG_THL_DLT  : constant := 16#B5#; -- digital BLC Satble Range
   --  16#B6# to 16#BD# -- reserved
   REG_AD_CHB   : constant := 16#BE#; -- bit[7] : reserved
                                      -- bit[6] : sign bit
                                      -- bit[5:0] : ADC offset value

   REG_AD_CHR   : constant := 16#BF#; -- bit[7] : reserved
                                      -- bit[6] : sign bit
                                      -- bit[5:0] : ADC offset value

   REG_AD_CHGb  : constant := 16#C0#; -- bit[7] : reserved
                                      -- bit[6] : sign bit
                                      -- bit[5:0] : ADC offset value

   REG_AD_CHGr  : constant := 16#C1#; -- bit[7] : reserved
                                      -- bit[6] : sign bit
                                      -- bit[5:0] : ADC offset value

   --  16#C2# to 16#C8# -- reserved

   REG_SATCTR   : constant := 16#C9#; -- saturation control



   ----------------------
   COM2_SSLEEP  : constant := 16#10#;   -- /*Soft sleep mode */
   COM3_SWAP    : constant := 16#40#;   -- /*Byte swap */
   COM3_SCALEEN : constant := 16#08#;   -- /*Enable scaling */
   COM3_DCWEN   : constant := 16#04#;   -- /*Enable downsamp/crop/window */
   CLK_EXT      : constant := 16#40#; -- /*Use external clock directly */
   CLK_SCALE    : constant := 16#3f#; -- /*Mask for internal clock scale */
   COM7_RESET   : constant := 16#80#; -- /*Register reset */
   COM7_FMT_MASK : constant := 16#38#;
   COM7_FMT_VGA : constant := 16#00#;
   COM7_FMT_CIF : constant := 16#20#; -- /*CIF format */
   COM1_CCIR656 : constant := 16#40#; --/*CCIR656 enable */


   COM7_FMT_QVGA : constant := 16#10#; -- /*QVGA format */
   COM7_FMT_QCIF : constant := 16#08#; -- /*QCIF format */
   COM7_RGB     : constant := 16#04#; -- /*bits 0 and 2 - RGB format */
   COM7_YUV     : constant := 16#00#; -- /*YUV */
   COM7_BAYER   : constant := 16#01#; -- /*Bayer format */
   COM7_PBAYER  : constant := 16#05#; -- /*"Processed bayer" */
   COM8_FASTAEC : constant := 16#80#; -- /*Enable fast AGC/AEC */
   COM8_AECSTEP : constant := 16#40#; -- /*Unlimited AEC step size */
   COM8_BFILT   : constant := 16#20#; -- /*Band filter enable */
   COM8_AGC     : constant := 16#04#; -- /*Auto gain enable */
   COM8_AWB     : constant := 16#02#; -- /*White balance enable */
   COM8_AEC     : constant := 16#01#; -- /*Auto exposure enable */
   COM10_HSYNC  : constant := 16#40#; -- /*HSYNC instead of HREF */
   COM10_PCLK_HB : constant := 16#20#; -- /*Suppress PCLK on horiz blank */
   COM10_HREF_REV : constant := 16#08#; -- /*Reverse HREF */
   COM10_VS_LEAD : constant := 16#04#; -- /*VSYNC on clock leading edge */
   COM10_VS_NEG : constant := 16#02#; -- /*VSYNC negative */
   COM10_HS_NEG : constant := 16#01#; -- /*HSYNC negative */
   MVFP_MIRROR  : constant := 16#20#; -- /*Mirror image */
   MVFP_FLIP    : constant := 16#10#; -- /*Vertical flip */


   TSLB_YLAST   : constant := 16#04#; -- /*UYVY or VYUY - see com13 */
   COM11_NIGHT  : constant := 16#80#; -- /*NIght mode enable */
   COM11_NMFR   : constant := 16#60#; -- /*Two bit NM frame rate */
   COM11_HZAUTO : constant := 16#10#; -- /*Auto detect 50/60 Hz */
   COM11_50HZ   : constant := 16#08#; -- /*Manual 50Hz select */
   COM11_EXP    : constant := 16#02#;
   COM12_HREF   : constant := 16#80#; -- /*HREF always */
   COM13_GAMMA  : constant := 16#80#; -- /*Gamma enable */
   COM13_UVSAT  : constant := 16#40#; -- /*UV saturation auto adjustment */
   COM13_UVSWAP : constant := 16#01#; -- /*V before U - w/TSLB */
   COM14_DCWEN  : constant := 16#10#; -- /*DCW/PCLK-scale enable */
   COM15_R10F0  : constant := 16#00#; -- /*Data range 10 to F0 */
   COM15_R01FE  : constant := 16#80#; -- /*           01 to FE */
   COM15_R00FF  : constant := 16#c0#; -- /*           00 to FF */
   COM15_RGB565 : constant := 16#10#; -- /*RGB565 output */
   COM15_RGB555 : constant := 16#30#; -- /*RGB555 output */
   COM16_AWBGAIN : constant := 16#08#; -- /*AWB gain enable */
   COM17_AECWIN : constant := 16#c0#; -- /*AEC window - must match COM4 */
   COM17_CBAR   : constant := 16#08#; -- /*DSP Color bar */


   --/*
   --  * This matrix defines how the colors are generated, must be
   --  * tweaked to adjust hue and saturation.
   --  *
   --  * Order: v-red, v-green, v-blue, u-red, u-green, u-blue
   --  *
   --  * They are nine-bit signed quantities, with the sign bit
   --  * stored in 16#58.  Sign for v-red is bit 0, and up from there.
   --  */

   REG_CMATRIX_BASE : constant := 16#4f#;
   R76_BLKPCOR  : constant := 16#80#; -- /*Black pixel correction enable */
   R76_WHTPCOR  : constant := 16#40#; -- /*White pixel correction enable */

   REG_RGB444   : constant := 16#8c#; -- /*RGB 444 control */
   R444_ENABLE  : constant := 16#02#; -- /*Turn on RGB444, overrides 5x5 */
   R444_RGBX    : constant := 16#01#; -- /*Empty nibble at end */

   REG_HAECC1   : constant := 16#9f#; -- /*Hist AEC/AGC control 1 */
   REG_HAECC2   : constant := 16#a0#; -- /*Hist AEC/AGC control 2 */

   REG_BD50MAX  : constant := 16#a5#; -- /*50hz banding step limit */
   REG_HAECC3   : constant := 16#a6#; -- /*Hist AEC/AGC control 3 */
   REG_HAECC4   : constant := 16#a7#; -- /*Hist AEC/AGC control 4 */
   REG_HAECC5   : constant := 16#a8#; -- /*Hist AEC/AGC control 5 */
   REG_HAECC6   : constant := 16#a9#; -- /*Hist AEC/AGC control 6 */
   REG_HAECC7   : constant := 16#aa#; -- /*Hist AEC/AGC control 7 */
   REG_BD60MAX  : constant := 16#ab#; -- /*60hz banding step limit */

   AWBC7           : constant := 16#59#; -- /*AWB Control 7 */
   AWBC8           : constant := 16#5a#; -- /*AWB Control 8 */
   AWBC9           : constant := 16#5b#; -- /*AWB Control 9 */
   AWBC10          : constant := 16#5c#; -- /*AWB Control 10 */
   AWBC11          : constant := 16#5d#; -- /*AWB Control 11 */
   AWBC12          : constant := 16#5e#; -- /*AWB Control 12 */


--
--     REG_GAIN               : constant := 16#00#;
--     REG_BLUE               : constant := 16#01#;
--     REG_RED                : constant := 16#02#;
--     REG_VREF              : constant := 16#03#;
--     REG_COM1 : constant := 16#04#;
--     COM1_CCIR656 : constant := 16#40#;
--     REG_BAVE               : constant := 16#05#;
--     REG_GbAVE               : constant := 16#06#;
--     REG_RAVE               : constant := 16#08#;
--     REG_COM2               : constant := 16#09#;
--     REG_PID                : constant := 16#0A#;
--     REG_VER                : constant := 16#0B#;
--     REG_COM3               : constant := 16#0C#;
--     REG_COM4               : constant := 16#0D#;
--     REG_COM5               : constant := 16#0E#;
--     REG_COM6               : constant := 16#0F#;
--     COM2_SSLEEP                : constant := 16#10#;
--     REG_CLKRC              : constant := 16#11#;
--     REG_COM7               : constant := 16#12#;
--     REG_COM8               : constant := 16#13#;
--     REG_COM9               : constant := 16#14#;
--     REG_COM10              : constant := 16#15#;
--     REG_REG16              : constant := 16#16#;
--     REG_HSTART             : constant := 16#17#;
--     REG_HSIZE              : constant := 16#18#;
--     REG_VSTART             : constant := 16#19#;
--     REG_VSIZE              : constant := 16#1A#;
--     REG_PSHFT              : constant := 16#1B#;
--     REG_MIDH               : constant := 16#1C#;
--     REG_MIDL               : constant := 16#1D#;
--     REG_LAEC               : constant := 16#1F#;
--     REG_COM11              : constant := 16#20#;
--     REG_BDBASE             : constant := 16#22#;
--     REG_DBSTEP             : constant := 16#23#;
--     REG_AEW                : constant := 16#24#;
--     REG_AEB                : constant := 16#25#;
--     REG_VPT                : constant := 16#26#;
--     REG_REG28              : constant := 16#28#;
--     REG_HOUTSIZE           : constant := 16#29#;
--     REG_EXHCH              : constant := 16#2A#;
--     REG_EXHCL              : constant := 16#2B#;
--     REG_VOUTSIZE           : constant := 16#2C#;
--     REG_ADVFL              : constant := 16#2D#;
--     REG_ADVFH              : constant := 16#2E#;
--     REG_YAVE               : constant := 16#2F#;
--     REG_LUMHTH             : constant := 16#30#;
--     REG_LUMLTH             : constant := 16#31#;
--     REG_HREF               : constant := 16#32#;
--     REG_DM_LNL             : constant := 16#33#;
--     REG_DM_LNH             : constant := 16#34#;
--     REG_ADOFF_B            : constant := 16#35#;
--     REG_ADOFF_R            : constant := 16#36#;
--     REG_ADOFF_GB           : constant := 16#37#;
--     REG_ADOFF_GR           : constant := 16#38#;
--     REG_OFF_B              : constant := 16#39#;
--     REG_OFF_R              : constant := 16#3A#;
--     REG_OFF_GB             : constant := 16#3B#;
--     REG_OFF_GR             : constant := 16#3C#;
--     REG_COM12              : constant := 16#3D#;
--     REG_COM13              : constant := 16#3E#;
--     REG_COM14              : constant := 16#3F#;
--     REG_COM15              : constant := 16#40#;
--     REG_COM16              : constant := 16#41#;
--     REG_TGT_B              : constant := 16#42#;
--     REG_TGT_R              : constant := 16#43#;
--     REG_TGT_GB             : constant := 16#44#;
--     REG_TGT_GR             : constant := 16#45#;
--     REG_LC_CTR             : constant := 16#46#;
--     REG_LC_XC              : constant := 16#47#;
--     REG_LC_YC              : constant := 16#48#;
--     REG_LC_COEF            : constant := 16#49#;
--     REG_LC_RADI            : constant := 16#4A#;
--     REG_LC_COEFB           : constant := 16#4B#;
--     REG_LC_COEFR           : constant := 16#4C#;
--     REG_FIXGAIN            : constant := 16#4D#;
--     REG_AREF0              : constant := 16#4E#;
--     REG_AREF1              : constant := 16#4F#;
--     REG_AREF2              : constant := 16#50#;
--     REG_AREF3              : constant := 16#51#;
--     REG_AREF4              : constant := 16#52#;
--     REG_AREF5              : constant := 16#53#;
--     REG_AREF6              : constant := 16#54#;
--     REG_AREF7              : constant := 16#55#;
--     REG_UFIX               : constant := 16#60#;
--     REG_VFIX               : constant := 16#61#;
--     REG_AWBB_BLK           : constant := 16#62#;
--     REG_AWB_CTRL0          : constant := 16#63#;
--     REG_DSP_CTRL1          : constant := 16#64#;
--     REG_DSP_CTRL2          : constant := 16#65#;
--     REG_DSP_CTRL3          : constant := 16#66#;
--     REG_DSP_CTRL4          : constant := 16#67#;
--     REG_AWB_BIAS           : constant := 16#68#;
--     REG_AWB_CTRL1          : constant := 16#69#;
--     REG_AWB_CTRL2          : constant := 16#6A#;
--     REG_AWB_CTRL3          : constant := 16#6B#;
--     REG_AWB_CTRL4          : constant := 16#6C#;
--     REG_AWB_CTRL5          : constant := 16#6D#;
--     REG_AWB_CTRL6          : constant := 16#6E#;
--     REG_AWB_CTRL7          : constant := 16#6F#;
--     REG_AWB_CTRL8          : constant := 16#70#;
--     REG_AWB_CTRL9          : constant := 16#71#;
--     REG_AWB_CTRL10         : constant := 16#72#;
--     REG_AWB_CTRL11         : constant := 16#73#;
--     REG_AWB_CTRL12         : constant := 16#74#;
--     REG_AWB_CTRL13         : constant := 16#75#;
--     REG_AWB_CTRL14         : constant := 16#76#;
--     REG_AWB_CTRL15         : constant := 16#77#;
--     REG_AWB_CTRL16         : constant := 16#78#;
--     REG_AWB_CTRL17         : constant := 16#79#;
--     REG_AWB_CTRL18         : constant := 16#7A#;
--     REG_AWB_CTRL19         : constant := 16#7B#;
--     REG_AWB_CTRL20         : constant := 16#7C#;
--     REG_AWB_CTRL21         : constant := 16#7D#;
--     REG_GAM1               : constant := 16#7E#;
--     REG_GAM2               : constant := 16#7F#;
--     REG_GAM3               : constant := 16#80#;
--     REG_GAM4               : constant := 16#81#;
--     REG_GAM5               : constant := 16#82#;
--     REG_GAM6               : constant := 16#83#;
--     REG_GAM7               : constant := 16#84#;
--     REG_GAM8               : constant := 16#85#;
--     REG_GAM9               : constant := 16#86#;
--     REG_GAM10              : constant := 16#87#;
--     REG_GAM11              : constant := 16#88#;
--     REG_GAM12              : constant := 16#89#;
--     REG_GAM13              : constant := 16#8A#;
--     REG_GAM14              : constant := 16#8B#;
--     REG_GAM15              : constant := 16#8C#;
--     REG_SLOP               : constant := 16#8D#;
--     REG_DNSTH              : constant := 16#8E#;
--     REG_EDGE0              : constant := 16#8F#;
--     REG_EDGE1              : constant := 16#90#;
--     REG_DNSOFF             : constant := 16#91#;
--     REG_EDGE2              : constant := 16#92#;
--     REG_EDGE3              : constant := 16#93#;
--     REG_MTX1               : constant := 16#94#;
--     REG_MTX2               : constant := 16#95#;
--     REG_MTX3               : constant := 16#96#;
--     REG_MTX4               : constant := 16#97#;
--     REG_MTX5               : constant := 16#98#;
--     REG_MTX6               : constant := 16#99#;
--     REG_MTX_CTRL           : constant := 16#9A#;
--     REG_MTX_CTRL_DBL_EN    : constant := 16#80#;
--     REG_BRIGHTNESS         : constant := 16#9B#;
--     REG_CONTRAST           : constant := 16#9C#;
--     REG_UVADJ0             : constant := 16#9E#;
--     REG_UVADJ1             : constant := 16#9F#;
--     REG_SCAL0              : constant := 16#A0#;
--     REG_SCAL1              : constant := 16#A1#;
--     REG_SCAL2              : constant := 16#A2#;
--     REG_FIFODLYM           : constant := 16#A3#;
--     REG_FIFODLYA           : constant := 16#A4#;
--     REG_SDE                : constant := 16#A6#;
--     REG_USAT               : constant := 16#A7#;
--     REG_VSAT               : constant := 16#A8#;
--     REG_HUECOS             : constant := 16#A9#;
--     REG_HUESIN             : constant := 16#AA#;
--     REG_SIGN_BIT           : constant := 16#AB#;
--     REG_DSPAUTO            : constant := 16#AC#;
--     REG_SDE_NEGATIVE_EN    : constant := 16#40#;
--     REG_SDE_GRAYSCALE_EN   : constant := 16#20#;
--     REG_SDE_V_FIXED_EN     : constant := 16#10#;
--     REG_SDE_U_FIXED_EN     : constant := 16#08#;
--     REG_SDE_CONT_BRIGHT_EN : constant := 16#04#;
--     REG_SDE_SATURATION_EN  : constant := 16#02#;
--     REG_SDE_HUE_EN         : constant := 16#01#;
--
--     COM3_SWAP_YUV      : constant := 16#10#;
--     COM7_RES_VGA       : constant := 16#00#;
--     COM7_FMT_RGB565    : constant := 16#04#;
--     COM7_FMT_RGB       : constant := 16#02#;
--     DSP_CTRL2_VDCW_EN  : constant := 16#08#;
--     DSP_CTRL2_HDCW_EN  : constant := 16#04#;
--     DSP_CTRL2_VZOOM_EN : constant := 16#02#;
--     DSP_CTRL2_HZOOM_EN : constant := 16#01#;
--     SDE_CONT_BRIGHT_EN : constant := 16#04#;
--     SDE_SATURATION_EN  : constant := 16#02#;

end OV7670;
