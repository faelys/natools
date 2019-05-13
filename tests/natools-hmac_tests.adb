------------------------------------------------------------------------------
-- Copyright (c) 2014-2019, Natacha Porté                                   --
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

with GNAT.MD5;
with GNAT.SHA1;
with GNAT.SHA256;
with Natools.GNAT_HMAC.MD5;
with Natools.GNAT_HMAC.SHA1;
with Natools.GNAT_HMAC.SHA256;
with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.Test_Tools;

package body Natools.HMAC_Tests is

   package Test_Tools renames Natools.S_Expressions.Test_Tools;

   generic
      type Context is private;
      with function HMAC_Initial_Context (Key : String) return Context;
      with procedure Update (C : in out Context; Input : S_Expressions.Atom);
      with function Digest (C : Context) return S_Expressions.Atom;
   function GNAT_HMAC_Digest (Key, Message : S_Expressions.Atom)
     return S_Expressions.Atom;

   function Hex_Atom (Hexadecimal : String) return S_Expressions.Atom;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function GNAT_HMAC_Digest (Key, Message : S_Expressions.Atom)
     return S_Expressions.Atom
   is
      C : Context := HMAC_Initial_Context (S_Expressions.To_String (Key));
   begin
      Update (C, Message);
      return Digest (C);
   end GNAT_HMAC_Digest;


   function GNAT_HMAC_MD5_Digest is new GNAT_HMAC_Digest
     (GNAT.MD5.Context,
      GNAT.MD5.HMAC_Initial_Context,
      GNAT.MD5.Update,
      GNAT.MD5.Digest);

   function GNAT_HMAC_SHA1_Digest is new GNAT_HMAC_Digest
     (GNAT.SHA1.Context,
      GNAT.SHA1.HMAC_Initial_Context,
      GNAT.SHA1.Update,
      GNAT.SHA1.Digest);

   function GNAT_HMAC_SHA256_Digest is new GNAT_HMAC_Digest
     (GNAT.SHA256.Context,
      GNAT.SHA256.HMAC_Initial_Context,
      GNAT.SHA256.Update,
      GNAT.SHA256.Digest);


   function Hex_Atom (Hexadecimal : String) return S_Expressions.Atom is
   begin
      return S_Expressions.Encodings.Decode_Hex
        (S_Expressions.To_Atom (Hexadecimal));
   end Hex_Atom;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      RFC_2104 (Report);
      RFC_4231 (Report);
      Wikipedia (Report);
      Vanilla_GNAT (Report);
   end All_Tests;


   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure RFC_2104 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("HMAC-MD5 test vectors from RFC-2104");
   begin
      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("9294727a3638bb1c13f48ef8158bfc9d"),
         GNAT_HMAC.MD5.Digest
           (S_Expressions.Atom'(1 .. 16 => 16#0b#),
            S_Expressions.To_Atom ("Hi There")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("750c783e6ab0b503eaa86e310a5db738"),
         GNAT_HMAC.MD5.Digest
           ("Jefe",
            S_Expressions.To_Atom ("what do ya want for nothing?")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("56be34521d144c88dbb8c733f0e8b3f6"),
         GNAT_HMAC.MD5.Digest
           (S_Expressions.Atom'(1 .. 16 => 16#aa#),
            (1 .. 50 => 16#dd#)));
   exception
      when Error : others => Test.Report_Exception (Error);
   end RFC_2104;


   procedure RFC_4231 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("HMAC-SHA256 test vectors from RFC-4231");
   begin
      declare
         Context : GNAT_HMAC.SHA256.Context;
      begin
         GNAT_HMAC.SHA256.Setup (Context,
            S_Expressions.Atom'(1 .. 20 => 16#0b#));
         GNAT_HMAC.SHA256.Update (Context, "Hi There");
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("b0344c61d8db38535ca8afceaf0bf12b"
                    & "881dc200c9833da726e9376c2e32cff7"),
            GNAT_HMAC.SHA256.Digest (Context));

         GNAT_HMAC.SHA256.Setup (Context, "Jefe");
         GNAT_HMAC.SHA256.Update (Context, "what do ya want for nothing?");
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("5bdcc146bf60754e6a042426089575c7"
                    & "5a003f089d2739839dec58b964ec3843"),
            GNAT_HMAC.SHA256.Digest (Context));

         GNAT_HMAC.SHA256.Setup (Context,
            S_Expressions.Atom'(1 .. 20 => 16#aa#));
         GNAT_HMAC.SHA256.Update (Context,
            S_Expressions.Atom'(1 .. 50 => 16#dd#));
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("773ea91e36800e46854db8ebd09181a7"
                    & "2959098b3ef8c122d9635514ced565fe"),
            GNAT_HMAC.SHA256.Digest (Context));

         GNAT_HMAC.SHA256.Setup (Context,
            Hex_Atom ("0102030405060708090a0b0c0d0e0f10111213141516171819"));
         GNAT_HMAC.SHA256.Update (Context,
            S_Expressions.Atom'(1 .. 50 => 16#CD#));
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("82558a389a443c0ea4cc819899f2083a"
                    & "85f0faa3e578f8077a2e3ff46729665b"),
            GNAT_HMAC.SHA256.Digest (Context));

         GNAT_HMAC.SHA256.Setup (Context,
            S_Expressions.Atom'(1 .. 131 => 16#AA#));
         GNAT_HMAC.SHA256.Update (Context,
            "Test Using Larger Than Block-Size Key - Hash Key First");
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("60e431591ee0b67f0d8a26aacbf5b77f"
                    & "8e0bc6213728c5140546040f0ee37f54"),
            GNAT_HMAC.SHA256.Digest (Context));

         GNAT_HMAC.SHA256.Setup (Context,
            S_Expressions.Atom'(1 .. 131 => 16#aa#));
         GNAT_HMAC.SHA256.Update (Context,
            "This is a test using a larger than block-size key and a larger"
            & " than block-size data. The key needs to be hashed before"
            & " being used by the HMAC algorithm.");
         Test_Tools.Test_Atom
           (Test,
            Hex_Atom ("9b09ffa71b942fcb27635fbcd5b0e944"
                    & "bfdc63644f0713938a7f51535c3a35e2"),
            GNAT_HMAC.SHA256.Digest (Context));
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end RFC_4231;


   procedure Vanilla_GNAT (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Test vanilla GNAT HMAC implementation");
   begin
      --  RFC 2104 test vectors

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("9294727a3638bb1c13f48ef8158bfc9d"),
         GNAT_HMAC_MD5_Digest
           (S_Expressions.Atom'(1 .. 16 => 16#0b#),
            S_Expressions.To_Atom ("Hi There")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("750c783e6ab0b503eaa86e310a5db738"),
         GNAT_HMAC_MD5_Digest
           (S_Expressions.To_Atom ("Jefe"),
            S_Expressions.To_Atom ("what do ya want for nothing?")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("56be34521d144c88dbb8c733f0e8b3f6"),
         GNAT_HMAC_MD5_Digest
           (S_Expressions.Atom'(1 .. 16 => 16#aa#),
            (1 .. 50 => 16#dd#)));

      --  RFC 4231 test vectors

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("b0344c61d8db38535ca8afceaf0bf12b"
                 & "881dc200c9833da726e9376c2e32cff7"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.Atom'(1 .. 20 => 16#0b#),
            S_Expressions.To_Atom ("Hi There")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("5bdcc146bf60754e6a042426089575c7"
                 & "5a003f089d2739839dec58b964ec3843"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.To_Atom ("Jefe"),
            S_Expressions.To_Atom ("what do ya want for nothing?")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("773ea91e36800e46854db8ebd09181a7"
                 & "2959098b3ef8c122d9635514ced565fe"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.Atom'(1 .. 20 => 16#aa#),
            S_Expressions.Atom'(1 .. 50 => 16#dd#)));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("82558a389a443c0ea4cc819899f2083a"
                 & "85f0faa3e578f8077a2e3ff46729665b"),
         GNAT_HMAC_SHA256_Digest
           (Hex_Atom ("0102030405060708090a0b0c0d0e0f10111213141516171819"),
            S_Expressions.Atom'(1 .. 50 => 16#CD#)));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("60e431591ee0b67f0d8a26aacbf5b77f"
                 & "8e0bc6213728c5140546040f0ee37f54"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.Atom'(1 .. 131 => 16#AA#),
            S_Expressions.To_Atom
              ("Test Using Larger Than Block-Size Key - Hash Key First")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("9b09ffa71b942fcb27635fbcd5b0e944"
                 & "bfdc63644f0713938a7f51535c3a35e2"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.Atom'(1 .. 131 => 16#aa#),
            S_Expressions.To_Atom
              ("This is a test using a larger than block-size key and a larger"
               & " than block-size data. The key needs to be hashed before"
               & " being used by the HMAC algorithm.")));

      --  Wikipedia non-null test vectors (since GNAT HMAC rejects empty keys)

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("80070713463e7749b90c2dc24911e275"),
         GNAT_HMAC_MD5_Digest
           (S_Expressions.To_Atom ("key"),
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9"),
         GNAT_HMAC_SHA1_Digest
           (S_Expressions.To_Atom ("key"),
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("f7bc83f430538424b13298e6aa6fb143"
                 & "ef4d59a14946175997479dbc2d1a3cd8"),
         GNAT_HMAC_SHA256_Digest
           (S_Expressions.To_Atom ("key"),
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Vanilla_GNAT;


   procedure Wikipedia (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Test vectors from Wikipedia");
   begin
      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("74e6f7298a9c2d168935f58c001bad88"),
         GNAT_HMAC.MD5.Digest
           (S_Expressions.Null_Atom,
            S_Expressions.Null_Atom));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("80070713463e7749b90c2dc24911e275"),
         GNAT_HMAC.MD5.Digest
           ("key",
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("fbdb1d1b18aa6c08324b7d64b71fb76370690e1d"),
         GNAT_HMAC.SHA1.Digest
           (S_Expressions.Null_Atom,
            S_Expressions.Null_Atom));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9"),
         GNAT_HMAC.SHA1.Digest
           ("key",
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("b613679a0814d9ec772f95d778c35fc5"
                 & "ff1697c493715653c6c712144292c5ad"),
         GNAT_HMAC.SHA256.Digest
           (S_Expressions.Null_Atom,
            S_Expressions.Null_Atom));

      Test_Tools.Test_Atom
        (Test,
         Hex_Atom ("f7bc83f430538424b13298e6aa6fb143"
                 & "ef4d59a14946175997479dbc2d1a3cd8"),
         GNAT_HMAC.SHA256.Digest
           ("key",
            S_Expressions.To_Atom
              ("The quick brown fox jumps over the lazy dog")));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Wikipedia;

end Natools.HMAC_Tests;
