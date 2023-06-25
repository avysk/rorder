module move
   implicit none
   private

   ! The squares on the cube are numbered from 1 to 48, starting from the
   ! upper/front/right corner, clockwise or from front to behind. The faces
   ! are chosen in the following order: upper, down, front, behind, right,
   ! left.
   type rotation_description
      ! the corner of the rotated face; -1 if no face (rotation of the
      ! middle lafer).
      integer :: corner
      ! The rotation are defined as an array; a square with index idx
      ! becomes a square with index idx+1; the last becomes the first.
      integer, dimension(12) :: rotation_description
   end type

   type, public :: rotation
      integer, dimension(48, 48), private :: matrix

   contains
      procedure, private :: id => init_as_id
      procedure :: from_string => init_from_string
      procedure :: order => determine_order
   end type

contains

   function desc(name)
      rotation_description :: desc
      character(:) name

      subroutine init_as_id(this)
         class(rotation), intent(out) :: this

         this%matrix = 0

         do concurrent i = 1, 48
            this%matrix(i, i) = 1
         end do

      end subroutine

      function determine_order(this), result(order)
         integer :: order = 1
         class(rotaton), intent(in) :: this
         class(rotation) :: tmp = this
         class(rotation) :: id

         id%id()

         do
            if (all(tmp == id)) then
               break
            end if
            order + = 1
            tmp%matrix = matmul(tmp%matrix, this%matrix)
         end do

      end function

      end module
