(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

structure Prim :> PRIM = struct

datatype t =
         Int of Int64.int
       | Float of Real64.real
       | String of string
       | Char of char

open Print.PD
open Print

fun p_t t =
    case t of
        Int n => string (Int64.toString n)
      | Float n => string (Real64.toString n)
      | String s => box [string "\"", string (String.toString s), string "\""]
      | Char ch => box [string "#\"", string (String.toString (String.str ch)), string "\""]

fun int2s n =
    if Int64.compare (n, Int64.fromInt 0) = LESS then
        "-" ^ Int64.toString (Int64.~ n) ^ "LL"
    else
        Int64.toString n ^ "LL"

fun int2s' n =
    if Int64.compare (n, Int64.fromInt 0) = LESS then
        "-" ^ Int64.toString (Int64.~ n)
    else
        Int64.toString n

val float2s = String.translate (fn #"~" => "-" | ch => str ch) o Real64.toString

fun toString t =
    case t of
        Int n => int2s' n
      | Float n => float2s n
      | String s => s
      | Char ch => str ch

fun pad (n, ch, s) =
    if size s >= n then
        s
    else
        str ch ^ pad (n-1, ch, s)

fun p_t_GCC t =
    case t of
        Int n => string (int2s n)
      | Float n => string (float2s n)
      | String s => box [string "\"", string (String.toCString s), string "\""]
      | Char ch => box [string "'", string (Char.toCString ch), string "'"]

fun equal x =
    case x of
        (Int n1, Int n2) => n1 = n2
      | (Float n1, Float n2) => Real64.== (n1, n2)
      | (String s1, String s2) => s1 = s2
      | (Char ch1, Char ch2) => ch1 = ch2

      | _ => false

fun compare (p1, p2) =
    case (p1, p2) of
        (Int n1, Int n2) => Int64.compare (n1, n2)
      | (Int _, _) => LESS
      | (_, Int _) => GREATER

      | (Float n1, Float n2) => Real64.compare (n1, n2)
      | (Float _, _) => LESS
      | (_, Float _) => GREATER 

      | (String n1, String n2) => String.compare (n1, n2)
      | (String _, _) => LESS
      | (_, String _) => GREATER

      | (Char ch1, Char ch2) => Char.compare (ch1, ch2)

end
