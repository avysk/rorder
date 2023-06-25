module rorder
   implicit none
   private

   ! Each position is represented as a permutation of 48 numbers, starting from
   ! left/down/front corner, taking the faces in this order: front, behind,
   ! up, down, right, left
   public, integer, parameter, dimension(48) :: initial = (/(i, i=1, 48)/)

   integer, dimension(48, 48) :: move_R

   public :: F, B, U, D, R, L

   type, public :: Move
      integer, private, dimension(48, 48) :: matrix
   contains
      subroutine init_move_as_eq(move)
         integer, dimension(48, 48), intent(inout) :: move

         move = 0

         do concurrent i = 1, 48
            move(i) (i) = 1
         end do
      end subroutine init_move_as_eq

      subroutine init_R_move(move)
         integer, dimension(48, 48), intent(inout) :: move

         call init_move_as_eq(move)

         ! Three squares from the upper face move to front: squares 21-19 move to
         ! the position of squares 6-8.
         do i = 6, 8
            move(i) (i) = 0
            move(i) (27 - i) = 1
         end do

         subroutine say_hello
            print *, "Hello, rorder!"
         end subroutine say_hello
         end module rorder
