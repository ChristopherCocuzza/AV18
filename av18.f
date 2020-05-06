C ***********************************************************************
	SUBROUTINE AV18 (p,rho)
C
C  Deuteron momentum distribution rho = u^2 + w^2, where u and w are
C  S- and D-state wave functions in momentum space, normalized s.t.
C  int dp p^2 rho(p) = 1.
C  Ref: Fantoni and Pandharipande, Nucl. Phys. A427 (1984) 473
C
C  Input file has rho normalized s.t. 4*pi int dp p^2 rho(p) = 1,
C  so that for output, rho needs to be multiplied by 4*pi.
C
C  p in 1/fm, rho in 1/fm^3
C
C.. Uses IMSL interpolation routine DQDVAL.
C.. For compilation on jlabs1:
C.. > use imsl
C.. > f77 -o objectfile file.f -R/site/vni/lib/lib.solaris
C..       -L/site/vni/lib/lib.solaris -limsl -lsocket -lnsl
C.. IMSL decommissioned 8/30/11
C
C ***********************************************************************
        IMPLICIT NONE
        INTEGER	ip,np
        PARAMETER (np=200)
        REAL*8  p,rho
        REAL*8  Parr(np),RHOarr(np),rho_int,dum
        LOGICAL readin /.FALSE./
	REAL*8	pi
	SAVE

C...Value of pi
        pi = 4*DATAN(1.D0)

	rho = 0.D0

        IF (readin) GO TO 123
C...Read data from file
        OPEN (10,FILE='/u/home/wmelnitc/Work/EMC/D/Wfn/av18.dat',
     &		FORM='FORMATTED')
	DO ip=1,9
          READ (10,*)
	ENDDO
        DO ip=1,np
	  READ (10,*) Parr(ip), RHOarr(ip)
        ENDDO
        CLOSE (10)
        readin = .TRUE.
        print *, '... AV18 data read ...'

 123	IF (p.LE.Parr(1)) rho = 4*pi * RHOarr(1)
	IF (p.GT.Parr(1) .AND. p.LE.Parr(np)) THEN
	  CALL Pinterp (Parr,RHOarr,np,p,rho_int,dum,2)
	  rho = 4*pi * rho_int
c     &    rho = 4*pi * DQDVAL(p,np,Parr,RHOarr,.FALSE.)
	ENDIF

c	print *, 'Parr(1),Parr(np)=',Parr(1),Parr(np)
c	print *, 'p,rho=',P, RHO

        RETURN
        END
