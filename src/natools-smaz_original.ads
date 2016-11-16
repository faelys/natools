------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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

with Ada.Characters.Latin_1;
with Natools.Smaz_256;
with Natools.Smaz_Original_Hash;

package Natools.Smaz_Original is
   pragma Pure;

   LF : constant Character := Ada.Characters.Latin_1.LF;
   CR : constant Character := Ada.Characters.Latin_1.CR;

   Dictionary : constant Natools.Smaz_256.Dictionary
     := (Last_Code => 253,
         Values_Last => 593,
         Variable_Length_Verbatim => True,
         Max_Word_Length => 7,
         Offsets => (2, 5, 6, 7, 8, 10, 11, 14, 15, 16, 17, 19, 20, 23, 25,
            27, 29, 31, 32, 35, 37, 39, 40, 42, 43, 45, 47, 49, 50, 52, 54, 56,
            59, 61, 64, 66, 68, 70, 71, 73, 76, 78, 80, 85, 86, 87, 89, 91, 95,
            96, 99, 101, 103, 105, 107, 110, 112, 113, 115, 116, 117, 119, 121,
            124, 127, 128, 130, 137, 140, 142, 145, 147, 150, 152, 154, 156,
            159, 161, 164, 166, 168, 169, 172, 173, 175, 177, 181, 183, 185,
            188, 189, 191, 193, 197, 199, 201, 203, 206, 208, 211, 216, 217,
            219, 223, 225, 228, 230, 233, 235, 236, 237, 239, 242, 245, 247,
            249, 252, 254, 257, 260, 262, 264, 267, 269, 272, 274, 277, 279,
            283, 285, 287, 289, 292, 295, 298, 301, 303, 305, 307, 309, 312,
            315, 318, 321, 323, 325, 328, 330, 333, 336, 338, 340, 342, 344,
            347, 351, 354, 356, 359, 362, 365, 368, 371, 373, 375, 377, 379,
            382, 385, 387, 390, 392, 395, 397, 400, 403, 406, 409, 411, 413,
            415, 418, 420, 422, 425, 428, 430, 432, 435, 437, 440, 443, 445,
            448, 450, 452, 454, 455, 459, 462, 464, 467, 470, 473, 474, 476,
            479, 482, 484, 487, 492, 495, 497, 500, 502, 504, 507, 510, 513,
            514, 516, 518, 519, 521, 523, 524, 526, 529, 531, 534, 536, 539,
            541, 544, 546, 548, 550, 552, 555, 557, 559, 561, 563, 566, 569,
            572, 575, 578, 581, 583, 584, 587, 590),
         Values => " theetaofoandinse r th tinhethhhe to" & CR & LF & "ls d a"
            & "anerc od on ofreof t , isuat   n orwhichfmasitthat" & LF & "wa"
            & "sen   wes an i" & CR & "f gpnd snd ed wedhttp://forteingy The "
            & "ctir hisst inarnt, toyng hwithlealto boubewere bseo enthang th"
            & "eir""hifrom fin deionmev.veallre rirois cof tareea. her mer  p"
            & "es bytheydiraicnots, d tat celah neas tioon n tiowe a om, as o"
            & "urlillchhadthise tg e" & CR & LF & " where coe oa us dss" & LF
            & CR & LF & CR & LF & CR & "="" be es amaonet tor butelsol e ss,n"
            & "oter waivhoe a rhats tnsch whtrut/havely ta ha ontha- latien p"
            & "e rethereasssi fowaecourwhoitszfors>otun<imth ncate><verad wel"
            & "yee nid clacil</rt widive,  itwhi magexe cmen.com",
         Hash => Natools.Smaz_Original_Hash.Hash'Access);

end Natools.Smaz_Original;
