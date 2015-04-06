;; Constraint definitions for Moxie
;; Copyright (C) 2009-2014 Free Software Foundation, Inc.
;; Contributed by Anthony Green <green@moxielogic.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

(define_constraint "A"
  "An absolute address."
  (and (match_code "mem")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")
	    (match_test "GET_CODE (XEXP (op, 0)) == CONST"))))

(define_constraint "B"
  "An offset address."
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == PLUS")))

(define_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))
		    && REGNO_OK_FOR_BASE_P (REGNO (XEXP (op, 0)))")))

(define_constraint "O"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "I08"
  "An 8-bit constant (0..255)"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 255")))

(define_constraint "I16"
  "A 16-bit constant (0..65535)"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 65535")))

(define_constraint "N"
  "A constant -(0..255)"
  (and (match_code "const_int")
       (match_test "ival >= -255 && ival <= 0")))

(define_register_constraint "Ybc" "BC_PAIR" "BC pair")
(define_register_constraint "Yde" "DE_PAIR" "DE pair")
(define_register_constraint "Yhl" "HL_PAIR" "HL pair")
(define_register_constraint "Yin" "INDEX_REGS" "Index registers")
(define_register_constraint "yA"  "GROUP_A" "A")
(define_register_constraint "yB"  "GROUP_B" "HL")
(define_register_constraint "yC"  "GROUP_C" "BC/DE/HL/SP")
(define_register_constraint "yD"  "GROUP_D" "IX")
(define_register_constraint "yE"  "GROUP_E" "BC/DE/IX/SP")
(define_register_constraint "yF"  "GROUP_F" "IY")
(define_register_constraint "yG"  "GROUP_G" "BC/DE/IY/SP")
