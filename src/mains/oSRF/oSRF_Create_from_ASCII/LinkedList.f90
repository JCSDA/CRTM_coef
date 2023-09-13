MODULE LinkedList
!
! Description:
! ============
! Linked list type implementation
!
! Author: P. Stegmann
! Date: 2020-12-29
!
IMPLICIT NONE

TYPE NODE
  REAL(KIND=8), DIMENSION(2) :: datum
  TYPE(NODE), POINTER :: next
END TYPE NODE

END MODULE LinkedList
