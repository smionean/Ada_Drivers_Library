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

package body OV7670 is
   type Addr_And_Data is record
      Addr, Data : UInt8;
   end record;

   type Command_Array is array (Natural range <>) of Addr_And_Data;

   Setup_Commands : constant Command_Array :=
     ((REG_RGB444, 16#00#),  -- disable RGB 444
      (REG_TSLB, 16#04#),  -- 0D = UYVY  04 = YUYV
      (REG_COM13, 16#88#),  -- connect to REG_TSLB
      --(REG_COM13  , 16#08#),--connect to REG_TSLB disable

      (REG_COM7, 16#04#),  -- RGB565 : RGB + color bar disable
      (REG_COM15, 16#D0#),  -- RGB565 : Set rgb565 with Full range 0xD0
      --(REG_COM7   , 16#01#),  -- RAWRGB : raw rgb bayer
      --(REG_COM15  , 16#C0#),  -- RAWRGB : Full range
      --(REG_COM7   , 16#00#),  -- YUV
      --(REG_COM15  , 16#D0#),  -- Full range

      (REG_COM3, 16#00#), -- NOT (qqvga OR qvga)

      (16#32#, 16#F6#), -- was B6
      (16#17#, 16#13#), -- HSTART
      (16#18#, 16#01#), -- HSTOP
      (16#19#, 16#02#), -- VSTART
      (16#1A#, 16#7A#), -- VSTOP
      (REG_VREF, 16#CA#), -- set 2 high GAIN MSB
      (REG_BRIGHT, 16#00#), -- 0x00(Brightness 0) - 0x18(Brightness +1) - 0x98(Brightness -1)
      (REG_CONTRAS, 16#40#), -- 0x40(Contrast 0) - 0x50(Contrast +1) - 0x38(Contrast -1)
      (16#B1#, 16#04#), -- really enable ABLC
      (REG_MTX1, 16#80#),
      (REG_MTX2, 16#80#),
      (REG_MTX3, 16#00#),
      (REG_MTX4, 16#22#),
      (REG_MTX5, 16#5E#),
      (REG_MTX6, 16#80#),
      (REG_MTXS, 16#9E#),
      (AWBC7, 16#88#),
      (AWBC8, 16#88#),
      (AWBC9, 16#44#),
      (AWBC10, 16#67#),
      (AWBC11, 16#49#),
      (AWBC12, 16#0E#),
      (REG_GFIX, 16#00#),
      (REG_AWBCTR3, 16#0A#),
      (REG_AWBCTR2, 16#55#),
      (REG_AWBCTR1, 16#11#),
      (REG_AWBCTR0, 16#9F#),
      (REG_COM16, COM16_AWBGAIN), -- disable auto denoise and edge enhance
      (16#4C#, 16#00#), -- disable denoise
      (16#76#, 16#00#), -- disable denoise
      (16#77#, 16#00#), -- disable denoise
      (16#7B#, 16#00#), -- brighten up shadows a bit end point 4
      (16#7C#, 16#00#), -- brighten up shadows a bit end point 8
      (REG_COM9, 16#6A#), -- max gain to 128x
      (16#74#, 16#10#), -- disable digital gain
      (16#11#, 16#04#)
     );

   function Read (This     : OV7670_Camera;
                  Mem_Addr : UInt8) return UInt8;

   procedure Write (This     : OV7670_Camera;
                    Mem_Addr : UInt8;
                    Data     : UInt8);

   ----------
   -- Read --
   ----------

   function Read (This     : OV7670_Camera;
                  Mem_Addr : UInt8) return UInt8
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 1);
   begin
      This.I2C.Master_Transmit (Addr    => This.Addr,
                                Data    => (1 => Mem_Addr),
                                Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.I2C.Master_Receive (Addr    => This.Addr,
                               Data    => Data,
                               Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      return Data (Data'First);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This     : OV7670_Camera;
                    Mem_Addr : UInt8;
                    Data     : UInt8)
   is
      Status : I2C_Status;
   begin
      This.I2C.Master_Transmit (Addr    => This.Addr,
                                Data    => (1 => Mem_Addr, 2 => Data),
                                Status  => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out OV7670_Camera;
      Addr : I2C_Address)
   is
   begin
      This.Addr := Addr;
      for Elt of Setup_Commands loop
         Write (This, Elt.Addr, Elt.Data);
      end loop;
   end Initialize;

   ----------------------
   -- Set_Pixel_Format --
   ----------------------

   procedure Set_Pixel_Format
     (This : OV7670_Camera;
      Pix  : Pixel_Format)
   is
      Reg : UInt8 := Read (This, REG_COM7);
   begin
      Reg := Reg and 2#1111_00_11#;
      case Pix is
         when Pix_RGB565 =>
            Reg := Reg or 2#0000_01_00#;
         when Pix_RGB555 =>
            Reg := Reg or 2#0000_10_00#;
         when Pix_RGB444 =>
            Reg := Reg or 2#0000_11_00#;
      end case;
      Write (This, REG_COM7, Reg);

      Reg := Read (This, REG_COM3);
      Write (This, REG_COM3, Reg and 2#0000_1000#); --  SWAP MSB/LSB
   end Set_Pixel_Format;

   --------------------
   -- Set_Frame_Size --
   --------------------

   procedure Set_Frame_Size
     (This : OV7670_Camera;
      Res  : OV2640.Frame_Size)
   is
      --  use type OV2640.Frame_Size;
      Width  : constant UInt16 := OV2640.Resolutions (Res).Width;
      Height : constant UInt16 := OV2640.Resolutions (Res).Height;
   begin
   --   Write (This, REG_HOUTSIZE, UInt8 (Shift_Right (Width, 2)));
   --   Write (This, REG_VOUTSIZE, UInt8 (Shift_Right (Height, 1)));

      Write (This, REG_EXHCH,
             UInt8 (Width and 2#0111#)
             or
               Shift_Left (UInt8 (Height and 2#0001#), 2));

--        if Res < OV2640.VGA then
--           Write (This, REG_DSPAUTO, 16#FF#);
--        else
--           Write (This, REG_DSPAUTO, 16#F3#);
--           Write (This, REG_SCAL0, 16#00#);
--           Write (This, REG_SCAL1, 16#00#);
--           Write (This, REG_SCAL2, 16#00#);
--        end if;
   end Set_Frame_Size;

   -------------
   -- Get_PID --
   -------------

   function Get_PID (This : OV7670_Camera) return UInt8 is
   begin
      return Read (This, REG_PID);
   end Get_PID;

end OV7670;
