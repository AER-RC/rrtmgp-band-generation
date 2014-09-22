module useful_constants
  implicit none
  real, parameter :: tzero=273.15,pzero=1013.25,rhofac=1.0006
  real, parameter :: alosmt=2.6867775e+19,boltz=1.38044e-19
  real, parameter :: avogad=6.022045e+23,wtwat=18.015
  real, parameter :: c1=18.9766,c2=-14.9595,c3=-2.4388
  real, parameter :: b=avogad/wtwat
  
end module

program write_tape5s

  use useful_constants
  
  implicit none

  ! Interface definitions
  interface
    real function broad(pinit,tinit,gasinit)
      real, intent(in) :: pinit,tinit
      real, dimension(7,1), intent(in) :: gasinit
    end function broad      

    real function vmrtoden(pinit,tinit,wvinit)
      real, intent(in) :: pinit,tinit,wvinit
    end function vmrtoden

    real function dentovmr(pinit,tinit,wvinit)
      real, intent(in) :: pinit,tinit,wvinit
    end function dentovmr
    
    real function satden(tinit)
      real, intent(in) :: tinit 
    end function satden

    subroutine compute_wavenumber(wavenumber1,wavenumber2,dvout)
      real, intent(inout) :: wavenumber1, wavenumber2
      real, intent(out) :: dvout
    end subroutine compute_wavenumber

    subroutine write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xcont,reclayerinfo)
      character(len=20), intent(in) :: reclayerinfo
      integer, intent(in) :: iunit
      real, dimension(7), intent(in) :: xcont
      real, intent(in) :: wavenumber1, wavenumber2, dvout
    end subroutine write_t5hdr
  end interface
  
  ! PARAMETER DECLARATIONS
  integer, parameter :: mxmol=9, mxl=200, mxref = 50
  integer, parameter :: ipthak=3
  real, parameter :: deltatint=15.0,ntpts=5

  ! cHARACTER DECLARATIONS
  character(len=20) :: fnum(99),nnum(9), rec_2_1_lo, rec_2_1_hi
  character(len=50) :: tape5

  ! INTEGER DECLARATION
  integer igas1_l,igas2_l,igas1_u,igas2_u,nmol
  integer iwvn
  integer ilev,nlev,levdup(1),levlow(1),iref
  integer im
  integer index,itemp
  integer iunit
  integer ndelt
  integer, dimension(mxmol,1) :: igas_minor_l,igas_minor_u
  real, dimension(7) :: xlcmapper,xucmapper,xlcmappee,xucmappee

  ! REAL DECLARATIONS 
  real :: junk
  real :: wavenumber1, wavenumber2, dvout
  real :: p,frac,dennum
  real, dimension(9) :: eta
  real, dimension(mxref) :: pref,tref
  real, dimension(mxmol,mxref) :: amol
  real, dimension(mxl) :: press,rh,t0
  real, dimension(mxl,ntpts) :: temp
  real, dimension(mxmol,mxl) :: w_orig
  real, dimension(mxmol,mxl,ntpts) :: wmol,wmapper,wmappee
  real, dimension(ntpts) :: deltat

  
  data eta/0.0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1.0/

  data nnum/'01','02','03','04','05','06','07','08','09'/

  data fnum/'01','02','03','04','05','06','07','08','09', &
    '10','11','12','13','14','15','16','17','18','19', &
    '20','21','22','23','24','25','26','27','28','29', &
    '30','31','32','33','34','35','36','37','38','39', &
    '40','41','42','43','44','45','46','47','48','49', &
    '50','51','52','53','54','55','56','57','58','59', &
    '60','61','62','63','64','65','66','67','68','69', &
    '70','71','72','73','74','75','76','77','78','79', &
    '80','81','82','83','84','85','86','87','88','89', &
    '90','91','92','93','94','95','96','97','98','99' / 

  ! Set continuum for mapper, this does not change for upper or lower atmosphere.
  data xlcmapper /1.0,1.0,1.0,1.0,1.0,1.0,0.0/
  data xucmapper /1.0,1.0,1.0,1.0,1.0,1.0,0.0/
  
  namelist /par/ wavenumber1,wavenumber2,igas1_l,igas2_l,igas1_u, &
    igas2_u,igas_minor_l,igas_minor_u,nmol 
 
  !The following data corresponds to an MLS standard atmosphere.
  include 'std_atmos.f90'  
  
  ! Read in Namelist that provides the information concerning wavelength and
  ! gases for each level.
  read(*,PAR)

  ! Preparing tape5 information; see specific information about wavenumber line coupling
  ! WARNING: Changes may need to be made depending on version of linefile in use
  call compute_wavenumber(wavenumber1,wavenumber2,dvout)

  ! Pressure Profile provides the grid on which the k's will be stored. These
  ! therefore provide the pressure layers used in TAPE5.
  open(10,FILE='PRESSURE.PROFILE',FORM='FORMATTED')
  read(10,'(i3)') nlev
  ! Read in elements from pressure profile
  do ilev = 1,nlev
    read(10,'g18.7') press(ilev)
  enddo
  close(10)

  ! Construct the temperature grid
  index=1
  ndelt=(ntpts-1)/2
  do itemp = -ndelt,ndelt
    deltat(index) = itemp*deltatint
    index=index+1
  enddo 

  ! Find the layer at which a switch will be made from "keying off" the
  ! amount of one species below to "keying off" of another above.  
  levdup = maxloc(press(1:nlev), MASK = press(1:nlev) .lt. 100.0)
  
  do ilev = 1,nlev
    p = press(ilev)
    if (p .ge. pref(1)) then
      t0(ilev) = tref(1)
      w_orig(1:nmol,1) = amol(1:nmol,1)*1.e-6
    else if (p .le. pref(mxref)) then
      t0(ilev) = tref(mxref)
      w_orig(1:nmol,mxref) = amol(1:nmol,mxref)*1.e-6      
    else
      levlow = minloc(pref, MASK=pref .gt. p)
      iref = levlow(1)
      frac = (alog(p)-alog(pref(iref)))/&
        (alog(pref(iref+1))-alog(pref(iref)))
      t0(ilev) = tref(iref) + frac*(tref(iref+1)-tref(iref))
		  w_orig(1:nmol,ilev) = 1.e-6*(amol(1:nmol,iref) + frac* &
        (amol(1:nmol,iref+1)-amol(1:nmol,iref)))
    endif  

    rh(ilev) = vmrtoden(p,t0(ilev),w_orig(1,ilev))/satden(t0(ilev))

    ! Expand the gases to be a function of temperature
    do itemp=1,ntpts 
      temp(ilev,itemp)=t0(ilev)+deltat(itemp)
      ! Continue modification to profile by requiring each temperature regime to have a wv
      ! equivalent to the standard level temperature rh.
      dennum = rh(ilev)*satden(temp(ilev,itemp))
      wmol(1,ilev,itemp)=dentovmr(p,temp(ilev,itemp),dennum)            
      ! Set other molecules to orig
      wmol(2:nmol,ilev,itemp) = w_orig(2:nmol,ilev)
    enddo 

  enddo 


  ! Set up additional tape5 info
  nmol = 7
  write(rec_2_1_lo,'(" 1 ",(i2),(i2),"   1.000000  ")') levdup(1),nmol
  write(rec_2_1_hi,'(" 1 ",(i2),(i2),"   1.000000  ")') nlev-levdup(1)+1,nmol  

  if (igas2_l .eq. 0) then

    ! Handle the continuum designation for mappee tape5
    xlcmappee=0.0
    select case (igas1_l)
      case (2)
        xlcmappee(3) = 1.0 !CO2
      case (3)
        xlcmappee(4) = 1.0 !O3
      case (7)
        xlcmappee(5) = 1.0 !O2
    end select

    ! Handling the mapper tape5
    index = 1
    wmapper = wmol
    do itemp = 1,ntpts
      tape5 = 'tape5-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmapper,rec_2_1_lo)      
      do ilev=1,levdup(1)
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp),broad(press(ilev),temp(ilev,itemp),&
          wmapper(1:7,ilev,itemp))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
    ! Handling the mappee tape5
    index = 1
    wmappee = 0.0 
    do itemp = 1,ntpts
      wmappee(igas1_l,1:nlev,itemp)=wmol(igas1_l,1:nlev,itemp)
      tape5 = 'tape5nc-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmappee,rec_2_1_lo)
      do ilev=1,levdup(1)
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp),broad(press(ilev),temp(ilev,itemp),&
          wmappee(1:7,ilev,itemp))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
  endif

  ! UPPER ATMOSPHERE

  if (igas2_u .eq. 0) then

    ! Handle the continuum designation for mappee tape5
    xlcmappee=0.0
    select case (igas1_u)
      case (2)
        xlcmappee(3) = 1.0 !CO2
      case (3)
        xlcmappee(4) = 1.0 !O3
      case (7)
        xlcmappee(5) = 1.0 !O2
    end select

    ! Handling the mapper tape5
    index = ntpts+1
    wmapper = wmol
    do itemp = 1,ntpts
      tape5 = 'tape5-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmapper,rec_2_1_lo)      
      do ilev=levdup(1),nlev
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp),broad(press(ilev),temp(ilev,itemp),&
          wmapper(1:7,ilev,itemp))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
    ! Handling the mappee tape5
    index = ntpts+1
    wmappee = 0.0 
    do itemp = 1,ntpts
      wmappee(igas1_l,1:nlev,itemp)=wmol(igas1_l,1:nlev,itemp)
      tape5 = 'tape5nc-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmappee,rec_2_1_lo)
      do ilev=levdup(1),nlev
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp),broad(press(ilev),temp(ilev,itemp),&
          wmappee(1:7,ilev,itemp))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
  endif  
end

subroutine compute_wavenumber(wavenumber1,wavenumber2,dvout)
  real, intent(inout) :: wavenumber1,wavenumber2
  real, intent(out) :: dvout

  integer :: iwvn
  real, dimension(10) :: wvn_lcouple

  data wvn_lcouple/612.0,619.0,667.0,677.0,714.0,722.0, &     
    735.0,742.0,791.0,796.0/ 

    !This adjustment is required by LBLRTM to generate the necessary optical depths.
    !Note that when line coupling present, must extend the LBLRTM calculations to
    !outside the boundary of this region.

    wavenumber1 = wavenumber1 - 5.0
    wavenumber2 = wavenumber2 + 5.0

    ! Resets the wavenumber range based on line-coupling limits
    do iwvn=1,10,2
    if (wavenumber1 .ge. wvn_lcouple(iwvn) .and. wavenumber1 .le. wvn_lcouple(iwvn+1)) &
        wavenumber1 = wvn_lcouple(iwvn)-5.0
    enddo

    do iwvn=1,10,2
      if (wavenumber2 .ge. wvn_lcouple(iwvn) .and. wavenumber2 .le. wvn_lcouple(iwvn+1)) &
        wavenumber2 = wvn_lcouple(iwvn+1)+5.0
    enddo
 
    ! Temporary solution for setting up number of points going into the calculation of k.
    if (wavenumber1.ge.125. .and. wavenumber1.lt.240.) then
      dvout=0.00002
    else if (wavenumber1.le.325) then
      dvout=0.00004
    else if (wavenumber1.le.1300.) then
      dvout=0.00005
    else if (wavenumber1.le.1700.) then
      dvout=0.0002
    else if (wavenumber1.le.2590.) then
      dvout=0.0003
    else if (wavenumber1.gt.2590.) then
      dvout=0.00045
    else 
      dvout = 3.0e-5
    endif
end subroutine compute_wavenumber

subroutine write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xcont,reclayerinfo)
  implicit none

  character(len=20), intent(in) :: reclayerinfo
  integer, intent(in) :: iunit
  real, dimension(7), intent(in) :: xcont  
  real, intent(in) :: wavenumber1,wavenumber2,dvout

  write(iunit,'("TAPE5 FOR MLS")') 
  write(iunit,'("1        2         3         4         5         6         7         8         9")') 
  write(iunit,'("123456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-")')   
  write(iunit,'("$ STANDARD MID-LATITUDE SUMMER ATMOSPHERE")')
  write(iunit,'(" HI=1 F4=1 CN=6 AE=0 EM=0 SC=0 FI=0 PL=0 TS=0 AM=0 MG=1 LA=0    1        00   00")')
  write(iunit,'(7f10.4)'),xcont
  write(iunit,'(2f10.3,70x,e10.3)') wavenumber1,wavenumber2,dvout
  write(iunit,'(a20,"MIDLATITUDE SUMM H1=   0.00 H2= 70.00   ANG=   0.000  LEN= 0 ")') &
    reclayerinfo

end subroutine write_t5hdr



real function broad(pinit,tinit,gasinit)
  use useful_constants
  implicit none

  real, intent(in) :: pinit,tinit
  real, dimension(7,1), intent(in) :: gasinit
  real :: rhotot,water,rhodry
  
  rhotot = rhofac*pinit/(boltz*tinit)
  water = gasinit(1,1)*rhotot/(1.+gasinit(1,1))
  rhodry = rhotot-water

  broad=rhodry*1.e5*(1.-sum(gasinit(2:7,1)))
end function 

real function vmrtoden(pinit,tinit,wvinit)
  use useful_constants
  implicit none  
  real, intent(in) :: pinit,tinit,wvinit
  real :: rhoair
  
  rhoair = alosmt*(pinit/pzero)*(tzero/tinit) 
  vmrtoden = (wvinit/(1.+wvinit))*rhoair
end

real function dentovmr(pinit,tinit,wvinit)
  use useful_constants
  implicit none
  real, intent(in) :: pinit,tinit,wvinit
  real :: rhoair
  
  rhoair = alosmt*(pinit/pzero)*(tzero/tinit)  
  dentovmr = wvinit/(rhoair-wvinit)
end

real function satden(tinit)
  use useful_constants
  implicit none
  real, intent(in) :: tinit
  real :: tfrac

  tfrac = tzero/tinit
  satden = tfrac*b*exp(c1+c2*tfrac+c3*tfrac**2)*1.0e-6
end function satden

