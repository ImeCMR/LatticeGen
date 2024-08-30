! This program will generate PDB files for simple cubic and body centered cubic lattices
!
  program latticegen
  implicit none
!
! Defining variables
!
  real, parameter:: n=3.0, m=3.0
  real, dimension(8):: scx, scy, scz
  real, dimension(9):: bccx, bccy, bccz
  integer:: i, j, k, sccount=0, bcccount=0
  real:: scddx, scddy, scddz, bccddx, bccddy, bccddz, scxx, scyy, sczz, bccxx, bccyy, bcczz
  character:: SCatomn ='A   ', SCrsname = 'A   ', BCCatomn = 'A   ', BCCrsname ='A   '  
!
! Open input files and create output files
!
! ***** Inputs *****
! sc.dat  - simple cubic lattice
! bcc.dat - body centered cubic lattice
!
! ***** Outputs *****
! sc.pdb  - simple cubic lattice
! bcc.pdb - body centered cubic lattice
!
  open(unit=10, file='sc.dat', status='old')
  open(unit=20, file='bcc.dat', status='old')
  open(unit=30, file='sc.pdb', status='unknown')
  open(unit=40, file='bcc.pdb', status='unknown')
!
! Read data for a unit cell
!
! ***** Simple cubic lattice *****
  do i = 1,8
    read(10,*) scx(i), scy(i), scz(i)
  end do
! Body centered cubic lattice
  do i = 1,9
    read(20,*) bccx(i), bccy(i), bccz(i)
  end do
!
! Replicating atoms
!
! ***** Simple cubic lattice *****
  do i = 1,6
    scddx = (i-1)*n
    scxx  =  scddx
    do j = 1,6
      scddy = (j-1)*n
      scyy  =  scddy
      do k = 1,6
        scddz = (k-1)*n
        sczz  = scddz
        sccount = sccount + 1
        write(30,500) sccount, SCatomn, SCrsname, sccount, scxx, scyy, sczz, 0.0, 0.0
      end do
    end do
  end do
  500 format('ATOM',2x,i5,2x,a4,a4,1x,i4,4x,3f8.3,2f6.2)
! 
! ***** Body centered cubic lattice ***** 
!
! For coner atoms of the cells
  do i = 1,6
    bccddx = (i-1)*n
    bccxx  = bccddx
    do j = 1,6
      bccddy = (j-1)*n
      bccyy  = bccddy
      do k = 1,6
        bccddz = (k-1)*n
        bcczz  = bccddz
        bcccount = bcccount + 1
        write(40,600) bcccount, BCCatomn, BCCrsname, bcccount, bccxx, bccyy, bcczz, 0.0, 0.0            
      end do
    end do
  end do
!
!
! For middle atoms of the cells
  do i = 1,5
    bccddx = (m*i)-(m/2)
    bccxx  = bccddx
    do j = 1,5
      bccddy = (m*j)-(m/2)
      bccyy  = bccddy
      do k = 1,5
        bccddz = (m*k)-(m/2)
        bcczz  = bccddz
        bcccount = bcccount + 1
        write(40,600) bcccount, BCCatomn, BCCrsname, bcccount, bccxx, bccyy, bcczz, 0.0, 0.0            
      end do
    end do
  end do
  600 format('ATOM',2x,i5,2x,a4,a4,1x,i4,4x,3f8.3,2f6.2)   
!
!
  write(6,*)'================================================'
  write(6,*)'                                                '
  write(6,*)'         Your PDB files are ready !             '
  write(6,*)'        Open PDB files using Rasmol             '
  write(6,*)'                                                '
  write(6,*)'================================================'
  end program latticegen
