------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Exceptions;

with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Encodings.Tests is

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Hexadecimal_Test (Report);
      Base64_Test (Report);
      User_Base64_Test (Report);
   end All_Tests;


   procedure Hexadecimal_Test (Report : in out NT.Reporter'Class) is
      All_Octets : Atom (1 .. 256);
   begin
      for I in All_Octets'Range loop
         All_Octets (I) := Octet (I - All_Octets'First);
      end loop;

      declare
         Name : constant String := "Decoding upper-case hexadecimal encoded";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            All_Octets,
            Decode_Hex (Encode_Hex (All_Octets, Upper)));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding lower-case hexadecimal encoded";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            All_Octets,
            Decode_Hex (Encode_Hex (All_Octets, Lower)));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding garbage-laced text";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            (16#01#, 16#23#, 16#45#, 16#67#, 16#89#,
             16#AB#, 16#CD#, 16#EF#, 16#AB#, 16#CD#, 16#EF#),
            Decode_Hex (All_Octets));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding an odd number of nibbles";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            (16#45#, 16#56#, 16#70#),
            Decode_Hex (To_Atom ("45 56 7")));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decode_Hex with non-hex-digit";
         Result : Octet;
      begin
         Result := Decode_Hex (180);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception raised. Result: " & Octet'Image (Result));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised");
      end;

      declare
         Name : constant String := "Overflow in Encode_Hex";
         Result : Octet;
      begin
         Result := Encode_Hex (16, Lower);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception raised. Result: " & Octet'Image (Result));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised");
      end;
   end Hexadecimal_Test;


   procedure Base64_Test (Report : in out NT.Reporter'Class) is
   begin
      declare
         Name : constant String := "Decoding encoding of all octet triplets";
         Success : Boolean := True;
         Expected : Atom (1 .. 3);
      begin
         for A in Octet loop
            Expected (1) := A;
            for B in Octet loop
               Expected (2) := B;
               for C in Octet loop
                  Expected (3) := C;

                  declare
                     Found : constant Atom
                       := Decode_Base64 (Encode_Base64 (Expected));
                  begin
                     if Expected /= Found then
                        if Success then
                           Success := False;
                           Report.Item (Name, NT.Fail);
                        end if;

                        Test_Tools.Dump_Atom (Report, Found, "Found");
                        Test_Tools.Dump_Atom (Report, Expected, "Expected");
                     end if;
                  end;
               end loop;
            end loop;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding encoding of all octet duets";
         Success : Boolean := True;
         Expected : Atom (1 .. 2);
      begin
         for A in Octet loop
            Expected (1) := A;
            for B in Octet loop
               Expected (2) := B;

               declare
                  Found : constant Atom
                    := Decode_Base64 (Encode_Base64 (Expected));
               begin
                  if Expected /= Found then
                     if Success then
                        Success := False;
                        Report.Item (Name, NT.Fail);
                     end if;

                     Test_Tools.Dump_Atom (Report, Found, "Found");
                     Test_Tools.Dump_Atom (Report, Expected, "Expected");
                  end if;
               end;
            end loop;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding encoding of all single octets";
         Success : Boolean := True;
         Expected : Atom (1 .. 1);
      begin
         for A in Octet loop
            Expected (1) := A;

            declare
               Found : constant Atom
                 := Decode_Base64 (Encode_Base64 (Expected));
            begin
               if Expected /= Found then
                  if Success then
                     Success := False;
                     Report.Item (Name, NT.Fail);
                  end if;

                  Test_Tools.Dump_Atom (Report, Found, "Found");
                  Test_Tools.Dump_Atom (Report, Expected, "Expected");
               end if;
            end;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decode_Base64 with non-base64-digit";
         Result : Octet;
      begin
         Result := Decode_Base64 (127);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception raised. Result: " & Octet'Image (Result));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised");
      end;

      declare
         Name : constant String := "Overflow in Encode_Base64";
         Result : Octet;
      begin
         Result := Encode_Base64 (64);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception raised. Result: " & Octet'Image (Result));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised");
      end;
   end Base64_Test;


   procedure User_Base64_Test (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Base-64 with user-defined charset");
   begin
      declare
         Digit_62 : constant Octet := Character'Pos ('-');
         Digit_63 : constant Octet := Character'Pos ('_');
            --  Charset for Base-64 URI (RFC 4648)

         Padding : constant Octet := Character'Pos ('|');

         Source : constant Atom
           := (4#0000#, 4#0100#, 4#2003#,   4#0100#, 4#1101#, 4#2013#,
               4#0200#, 4#2102#, 4#2023#,   4#0300#, 4#3103#, 4#2033#,
               4#1001#, 4#0110#, 4#2103#,   4#1101#, 4#1111#, 4#2113#,
               4#1201#, 4#2112#, 4#2123#,   4#1301#, 4#3113#, 4#2133#,
               4#2002#, 4#0120#, 4#2203#,   4#2102#, 4#1121#, 4#2213#,
               4#2202#, 4#2122#, 4#2223#,   4#2302#, 4#3123#, 4#2233#,
               4#3003#, 4#0130#, 4#2303#,   4#3103#, 4#1131#, 4#2313#,
               4#3203#, 4#2132#, 4#2323#,   4#3303#, 4#3133#, 4#2333#,
               16#42#);
         Expected : constant Atom
           := (65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
               78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
               97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
               110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, Digit_62, Digit_63,
               81, 103, Padding, Padding);
         Encoded_Short : constant Atom
           := Encode_Base64 (Source, Digit_62, Digit_63);
         Encoded_Long : constant Atom
           := Encode_Base64 (Source, Digit_62, Digit_63, Padding);
      begin
         Test_Tools.Test_Atom (Test, Expected, Encoded_Long);

         Test_Tools.Test_Atom
           (Test,
            Expected (Expected'First .. Expected'Last - 2),
            Encoded_Short);

         Test_Tools.Test_Atom
           (Test,
            Source,
            Decode_Base64 (Encoded_Long, Digit_62, Digit_63));

         Test_Tools.Test_Atom
           (Test,
            Source,
            Decode_Base64 (Encoded_Short, Digit_62, Digit_63));
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end User_Base64_Test;

end Natools.S_Expressions.Encodings.Tests;
