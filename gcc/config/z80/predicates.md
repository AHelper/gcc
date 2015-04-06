;; Predicate definitions for Moxie
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
;; Predicates
;; -------------------------------------------------------------------------

;; Nonzero if OP can be source of a simple move operation.

(define_predicate "z80_general_movsrc_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
  /* Any (MEM LABEL_REF) is OK.  That is a pc-relative load.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (MEM_P (op)
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST)
    return 1;

  return general_operand (op, mode);
})
(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
               || GET_CODE (XEXP (op, 0)) == LABEL_REF)
              && GET_CODE (XEXP (op, 1)) == CONST_INT);

    default:
      return false;
    }
})
;; Nonzero if OP can be an operand to an add/inc/dec instruction.

(define_predicate "z80_add_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), -255, 255)"))))

;; Nonzero if OP can be an operand to an sub/dec instruction.

(define_predicate "z80_sub_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), 0, 255)"))))
(define_predicate "mem_only"
  (match_code "mem"))
  
(define_predicate "shift_rotate_count"
;;  (ior (match_code "reg")
       (and (match_code "const_int")
	    (match_test "IN_RANGE (INTVAL (op), 1, 1)")))
;;	    )