module useful_constants
  implicit none
  real, parameter :: tzero=273.15,pzero=1013.25,rhofac=1.0006
  real, parameter :: alosmt=2.6867775e+19,boltz=1.38044e-19
  real, parameter :: avogad=6.022045e+23,wtwat=18.015
  real, parameter :: c1=18.9766,c2=-14.9595,c3=-2.4388
  real, parameter :: b=avogad/wtwat

  ! PARAMETER DECLARATIONS
  integer, parameter :: mxmol=9, mxl=200, mxref = 50
  integer, parameter :: ntpts=5,neta=9,nmol=7
  integer, parameter :: ipthak=3
  real, parameter :: deltatint=15.0, overthresh = 1.05

  ! Data declarations
  character(len=20) :: fnum(99),nnum(9)
  real, dimension(neta) :: eta
    
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

  contains
    real function broad(pinit,tinit,gasinit)
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
      implicit none  
      real, intent(in) :: pinit,tinit,wvinit
      real :: rhoair
  
      rhoair = alosmt*(pinit/pzero)*(tzero/tinit) 
      vmrtoden = (wvinit/(1.+wvinit))*rhoair
    end

    real function dentovmr(pinit,tinit,wvinit)
      implicit none
      real, intent(in) :: pinit,tinit,wvinit
      real :: rhoair
  
      rhoair = alosmt*(pinit/pzero)*(tzero/tinit)  
      dentovmr = wvinit/(rhoair-wvinit)
    end

    real function satden(tinit)
      implicit none
      real, intent(in) :: tinit
      real :: tfrac

      tfrac = tzero/tinit
      satden = tfrac*b*exp(c1+c2*tfrac+c3*tfrac**2)*1.0e-6
    end function satden

end module

program write_tape5s

  use useful_constants
  
  implicit none

  ! Interface definitions
  interface

    subroutine calculate_twokey_nowater(ileva,ilevb,ig1,ig2,win,wmapper,wmappee)
      integer, intent(in) :: ileva,ilevb,ig1,ig2
      real, dimension(mxmol,mxl,ntpts,neta), intent(in) :: win
      real, dimension(mxmol,mxl,ntpts,neta), intent(out) :: wmapper,wmappee
    end subroutine calculate_twokey_nowater

    subroutine calculate_twokey_water(ileva,ilevb,ig1,ig2,pin,tin,win,wmapper,wmappee)
      integer, intent(in) :: ileva,ilevb,ig1,ig2
      real, dimension(mxl), intent(in) :: pin
      real, dimension(mxl,ntpts), intent(in) :: tin
      real, dimension(mxmol,mxl,ntpts,neta), intent(in) :: win
      real, dimension(mxmol,mxl,ntpts,neta), intent(out) :: wmapper,wmappee
    end subroutine calculate_twokey_water
    
    subroutine compute_wavenumber(wavenumber1,wavenumber2,dvout)
      real, intent(inout) :: wavenumber1, wavenumber2
      real, intent(out) :: dvout
    end subroutine compute_wavenumber

    subroutine setup_continuum(ig1,ig2,xcmapper,xcmappee)
      integer, intent(in) :: ig1,ig2
      real, dimension(7,neta), intent(out) :: xcmapper,xcmappee     
    end subroutine setup_continuum

    subroutine write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xcont,reclayerinfo)
      character(len=20), intent(in) :: reclayerinfo
      integer, intent(in) :: iunit
      real, dimension(7), intent(in) :: xcont
      real, intent(in) :: wavenumber1, wavenumber2, dvout
    end subroutine write_t5hdr
  end interface

  ! cHARACTER DECLARATIONS
  character(len=20) :: rec_2_1_lo, rec_2_1_hi
  character(len=50) :: tape5

  ! INTEGER DECLARATION
  integer igas1_l,igas2_l,igas1_u,igas2_u
  integer iwvn,imol,im
  integer ilev,nlev,levdup(1),levlow(1),iref
  integer index,index2,itemp,ieta
  integer iunit
  integer ndelt
  integer, dimension(mxmol,1) :: igas_minor_l,igas_minor_u
  real, dimension(7,neta) :: xlcmapper,xucmapper,xlcmappee,xucmappee

  ! REAL DECLARATIONS 

  real :: wavenumber1, wavenumber2, dvout
  real :: p,frac, dennum
  real, dimension(mxref) :: pref,tref
  real, dimension(mxmol,mxref) :: amol
  real, dimension(mxl) :: press,rh,t0
  real, dimension(mxl,ntpts) :: temp
  real, dimension(mxmol,mxl) :: w_orig
  real, dimension(mxmol,mxl,ntpts,neta) :: wmol,wmoltwo,wmapper,wmappee
  real, dimension(ntpts) :: deltat
  
  namelist /par/ wavenumber1,wavenumber2,igas1_l,igas2_l,igas1_u, &
    igas2_u,igas_minor_l,igas_minor_u
 
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
    !! Deal now with one key species
    do itemp=1,ntpts 
      temp(ilev,itemp)=t0(ilev)+deltat(itemp)
      ! Continue modification to profile by requiring each temperature regime to have a wv
      ! equivalent to the standard level temperature rh.
      dennum = rh(ilev)*satden(temp(ilev,itemp))
      ! When one key species (wmol), adjust the water vapor
      wmol(1,ilev,itemp,neta)=dentovmr(p,temp(ilev,itemp),dennum)            
      ! Set other molecules to orig
      wmol(2:nmol,ilev,itemp,neta) = w_orig(2:nmol,ilev)  
      ! Also set up necessary seeds for two key species 
      wmoltwo(1:nmol,ilev,itemp,neta) = w_orig(1:nmol,ilev)
    enddo 

  enddo 

  ! Set up additional tape5 info
  write(rec_2_1_lo,'(" 1 ",(i2),(i2),"   1.000000  ")') levdup(1),nmol
  write(rec_2_1_hi,'(" 1 ",(i2),(i2),"   1.000000  ")') nlev-levdup(1)+1,nmol  

  ! Setup continuum - lower and upper
  call setup_continuum(igas1_l,igas2_l,xlcmapper,xlcmappee)
  call setup_continuum(igas1_u,igas2_u,xucmapper,xucmappee)
  
  if (igas1_l .ne. 0 .and. igas2_l .eq. 0) then
  ! LOWER ATMOSPHERE, ONE KEY SPECIES

    ! Handling the mapper tape5
    index = 1
    wmapper = wmol
    do itemp = 1,ntpts
      tape5 = 'tape5-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmapper(1:7,neta),rec_2_1_lo)      
      do ilev=1,levdup(1)
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp,neta),broad(press(ilev),&
          temp(ilev,itemp),wmapper(1:7,ilev,itemp,neta))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
    index = 1
    wmappee = 0.0 
    do itemp = 1,ntpts
      wmappee(igas1_l,1:levdup(1),itemp,neta)=wmol(igas1_l,1:levdup(1),itemp,neta)
      tape5 = 'tape5nc-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmappee(1:7,neta),rec_2_1_lo)
      do ilev=1,levdup(1)
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp,neta),broad(press(ilev),&
          temp(ilev,itemp),wmappee(1:7,ilev,itemp,neta))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
  else if (igas1_l .ne. 0 .and. igas2_l .ne. 0) then
  ! LOWER ATMOSPHERE, TWO KEY SPECIES    

  !! LOWER ATMOSPHERE, TWO KEY SPECIES, NEITHER IS WATER VAPOR
    if (igas1_l .ne. 1 .and. igas2_l .ne. 1) then
      print*,'LOWER: using calculate_twokey_nowater'
      call calculate_twokey_nowater(1,levdup(1),1,igas1_l,igas2_l,wmoltwo,wmapper,wmappee)      
    else if (igas1_l .eq. 1) then
      !! LOWER ATMOSPHERE, TWO KEY SPECIES, ONE IS WATER VAPOR
      print*,'LOWER: using calculate_twokey_water'
      call calculate_twokey_water(1,levdup(1),1,igas1_l,igas2_l,press,temp,wmoltwo,wmapper,wmappee)            
    else
      print*,'WARNING: igas1_l ne 1, not allowed'
      stop
    endif
    
    ! LOWER ATMOSPHERE, TWO KEY SPECIES
      ! Handling the mapper tape5
      do ieta = 1,9
        index = 1
        do itemp = 1,ntpts
          tape5 = 'tape5-T'//trim(fnum(index))//'-n'//trim(fnum(ieta))
          iunit=20
          open(iunit,FILE=tape5,FORM='FORMATTED')
          call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmapper(1:7,ieta),rec_2_1_lo)      
          do ilev=1,levdup(1)
            write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
            write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp,ieta),broad(press(ilev),&
              temp(ilev,itemp),wmapper(1:7,ilev,itemp,ieta))
          enddo
          close(iunit)
          index=index+1
        enddo 
      enddo
      
      do ieta = 1,9
        index = 1
        do itemp = 1,ntpts
          tape5 = 'tape5nc-T'//trim(fnum(index))//'-n'//trim(fnum(ieta))
          iunit=20
          open(iunit,FILE=tape5,FORM='FORMATTED')
          call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xlcmappee(1:7,ieta),rec_2_1_lo)      
          do ilev=1,levdup(1)
            write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
            write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp,ieta),broad(press(ilev),&
              temp(ilev,itemp),wmappee(1:7,ilev,itemp,ieta))
          enddo
          close(iunit)
          index=index+1
        enddo 
      enddo
  end if

  !!! UPPER ATMOSPHERE

  if (igas1_u .ne. 0 .and. igas2_u .eq. 0) then
  ! UPPER ATMOSPHERE, ONE KEY SPECIES
    ! Handling the mapper tape5
    index = ntpts+1
    wmapper = wmol
    do itemp = 1,ntpts
      tape5 = 'tape5-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xucmapper(1:7,9),rec_2_1_hi)      
      do ilev=levdup(1),nlev
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp,neta),broad(press(ilev),&
          temp(ilev,itemp),wmapper(1:7,ilev,itemp,neta))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
    ! Handling the mappee tape5
    index = ntpts+1
    wmappee = 0.0 
    do itemp = 1,ntpts
      wmappee(igas2_u,levdup(1):nlev,itemp,neta)=wmol(igas2_u,levdup(1):nlev,itemp,neta)
      tape5 = 'tape5nc-T'//trim(fnum(index))//'-n09'
      iunit=20
      open(iunit,FILE=tape5,FORM='FORMATTED')
      call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xucmappee(1:7,9),rec_2_1_hi)
      do ilev=levdup(1),nlev
        write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
        write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp,neta),broad(press(ilev),&
          temp(ilev,itemp),wmappee(1:7,ilev,itemp,neta))
      enddo
      close(iunit)
      index=index+1
    enddo  
        
  else if (igas1_u .ne. 0 .and. igas2_u .ne. 0) then
    if (igas1_u .ne. 1 .and. igas2_u .ne. 1) then
      !! UPPER ATMOSPHERE, TWO KEY SPECIES, NEITHER IS WATER VAPOR
      print*,'UPPER: using calculate_twokey_nowater '
      call calculate_twokey_nowater(levdup(1),nlev,2,igas1_u,igas2_u,wmoltwo,wmapper,wmappee)   
    else if (igas1_u .eq. 1) then 
      !! UPPER ATMOSPHERE, TWO KEY SPECIES, ONE IS WATER VAPOR
      print*,'UPPER: using calculate_twokey_water '
      call calculate_twokey_water(levdup(1),nlev,2,igas1_u,igas2_u,press,temp,wmoltwo,wmapper,wmappee)   
    else
      print*,'WARNING: Something wrong with upper atmosphere input'
      stop
    end if 

      do ieta = 1,neta,2
        index = 6
        do itemp = 1,ntpts
          tape5 = 'tape5-T'//trim(fnum(index))//'-n'//trim(fnum(ieta))
          iunit=20
          open(iunit,FILE=tape5,FORM='FORMATTED')
          call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xucmapper(1:7,ieta),rec_2_1_lo)      
          do ilev=levdup(1),nlev
            write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
            write(iunit,'(1P,8G15.7,0P)') wmapper(1:7,ilev,itemp,ieta),broad(press(ilev),&
              temp(ilev,itemp),wmapper(1:7,ilev,itemp,ieta))
          enddo
          close(iunit)
          index=index+1
        enddo 
      enddo
      
      do ieta = 1,neta,2
        index = 6
        do itemp = 1,ntpts
          tape5 = 'tape5nc-T'//trim(fnum(index))//'-n'//trim(fnum(ieta))
          iunit=20
          open(iunit,FILE=tape5,FORM='FORMATTED')
          call write_t5hdr(iunit,wavenumber1,wavenumber2,dvout,xucmappee(1:7,ieta),rec_2_1_lo)      
          do ilev=levdup(1),nlev
            write(iunit,'1P,G15.7,G10.5,13X,I2,0P') press(ilev),temp(ilev,itemp),ipthak
            write(iunit,'(1P,8G15.7,0P)') wmappee(1:7,ilev,itemp,ieta),broad(press(ilev),&
              temp(ilev,itemp),wmappee(1:7,ilev,itemp,ieta))
          enddo
          close(iunit)
          index=index+1
        enddo 
      enddo
    end if
end 

!! LOWER ATMOSPHERE, TWO KEY SPECIES, NEITHER IS WATER VAPOR
subroutine calculate_twokey_nowater(ileva,ilevb,ig1,ig2,win,wmapper,wmappee)
  use useful_constants
  implicit none
  
  integer, intent(in) :: ileva,ilevb,ig1,ig2
  real, dimension(mxmol,mxl,ntpts,neta), intent(in) :: win
  real, dimension(mxmol,mxl,ntpts,neta), intent(out) :: wmapper,wmappee
  
  integer :: ieta,itemp,ilev

  if (ig1 .eq. 1 .or. ig2 .eq. 1) then 
    print*,'calculate_twokey_nowater: WATER VAPOR KEY SPECIES NOT ALLOWED IN THIS SUBROUTINE. INVALID igas #',ig1,ig2
    stop
  end if
    
  do ieta=1,neta
    do itemp=1,ntpts
      do ilev=ileva,ilevb
        wmapper(1:nmol,ilev,itemp,ieta) = win(1:nmol,ilev,itemp,9)
        wmappee(1:nmol,ilev,itemp,ieta) = 0.0
        if (ieta .eq. 1) then 
          wmapper(ig1,ilev,itemp,ieta) = 0.0
          wmappee(ig2,ilev,itemp,ieta) = wmapper(ig2,ilev,itemp,ieta)            
        else if (ieta .eq. 9) then
          wmappee(ig1,ilev,itemp,ieta) = win(ig1,ilev,itemp,9)              
          wmapper(ig2,ilev,itemp,ieta) = 0.0
        else
          wmapper(ig1,ilev,itemp,ieta) = win(ig1,ilev,itemp,9) * &
          eta(ieta)/(1.0-eta(ieta))
          wmappee(ig1,ilev,itemp,ieta) = wmapper(ig1,ilev,itemp,ieta)
          wmappee(ig2,ilev,itemp,ieta) = wmapper(ig2,ilev,itemp,ieta)                             
        end if    
      enddo
    end do
  end do
end subroutine calculate_twokey_nowater

subroutine calculate_twokey_water(ileva,ilevb,ig1,ig2,pin,tin,win,wmapper,wmappee)
  use useful_constants
  implicit none
  
  integer, intent(in) :: ileva,ilevb,ig1,ig2
  real, dimension(mxl), intent(in) :: pin
  real, dimension(mxl,ntpts), intent(in) :: tin
  real, dimension(mxmol,mxl,ntpts,neta), intent(in) :: win
  real, dimension(mxmol,mxl,ntpts,neta), intent(out) :: wmapper,wmappee

  integer :: ieta,itemp,ilev
  real :: dennum,denrat,densat,densatover,wsat,wsatover,wetahigh,wtst
  
  if (ig1 .ne. 1 .or. ig2 .eq. 1) then 
    print*,'calculate_twokey_water: FIRST KEY SPECIES IS NOT WATER VAPOR.  INVALID igas #',ig1,ig2
    stop
  end if

  do ilev=ileva,ilevb
    ! WARNING: BE CAREFUL TO CHANGE THIS AS NEEDED
    wetahigh=win(1,ilev,1,9)*(0.99/(1.0-0.99))
    do itemp=1,ntpts
      densat = satden(tin(ilev,itemp))
      densatover = overthresh*densat
      wsat = dentovmr(pin(ilev),tin(ilev,itemp),densat)
      wsatover = dentovmr(pin(ilev),tin(ilev,itemp),densatover)
      do ieta=1,neta
        ! Set all gases to their standard, normal values
        wmapper(1:nmol,ilev,itemp,ieta) = win(1:nmol,ilev,itemp,9)
        wmappee(1:nmol,ilev,itemp,ieta) = 0.0
        if (ieta .eq. 1) then
          ! Set WV (igas1_l) to 0; leave other key species as normal 
          wmapper(1,ilev,itemp,ieta) = 0.0
          wmappee(ig2,ilev,itemp,ieta) = wmapper(ig2,ilev,itemp,ieta)
        else if (ieta .eq. 9) then
          dennum=vmrtoden(pin(ilev),tin(ilev,itemp),wetahigh)
          denrat = dennum/densat
          if (denrat .gt. overthresh) then
            wmapper(1,ilev,itemp,ieta) = wsatover
          endif             
          wmapper(ig2,ilev,itemp,ieta) = 0.0
          wmappee(1,ilev,itemp,ieta) = wmapper(1,ilev,itemp,ieta)
        else
          wtst = wmapper(1,ilev,itemp,ieta)* &
            eta(ieta)/(1.0-eta(ieta))
          dennum = vmrtoden(pin(ilev),tin(ilev,itemp),wtst)
          denrat = dennum/densat
          if (denrat .gt. overthresh) then
            ! Readjust the second key species now that we have to adjust water vapor.
            wmapper(ig2,ilev,itemp,ieta) = &
              ((1.0-eta(ieta))/eta(ieta))* &
              (wmapper(ig2,ilev,itemp,ieta)/ &
              wmapper(1,ilev,itemp,ieta))* &
              wsatover
              ! Set water vapor to the threshold saturation value.
            wmapper(1,ilev,itemp,ieta) = wsatover   
          else
            wmapper(1,ilev,itemp,ieta) = wtst
          endif 
          wmappee(1,ilev,itemp,ieta) = wmapper(1,ilev,itemp,ieta)
          wmappee(ig2,ilev,itemp,ieta) = wmapper(ig2,ilev,itemp,ieta)                    
        end if    
      enddo
    end do
  end do     
    
end subroutine calculate_twokey_water

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

subroutine setup_continuum(ig1,ig2,xcmapper,xcmappee)
  use useful_constants
  implicit none
  
  integer, intent(in) :: ig1,ig2
  real, dimension(7,neta), intent(out) :: xcmapper,xcmappee

  ! NOTE: When there is one key species, it is equivalent to the case for
  ! eta=9 with two key species.  For simplicity, we just fill the array as
  ! if there were two key species even when there isn't, but those values
  ! are not used.
  xcmapper = 1.0
  xcmapper(7,1:neta) = 0.0 
  xcmappee = 0.0
  xcmappee(7,1:neta) = 0.0 
  
  if (ig1 .eq. ig2) then
    print*,'WARNING: BAND GASES CANNOT BE THE SAME'
    xcmapper=-99.0
    xcmappee=-99.0
  end if 

  select case (ig1)
    case (1)
      ! H2O, Turn off WV continuum for eta=1 case
      xcmapper(1:2,1) = 0.0
      ! H2O mappee continuum treated separately; contribution not
      ! included in the ks.
      xcmappee(1:2,2:neta) = 0.0
    case (2)
      ! CO2, Turn off CO2 mapper continuum for eta=1 case
      xcmapper(3,1) = 0.0
      ! CO2, Turn on CO2 mappee continuum as contribution
      ! included in the ks
      xcmappee(3,2:neta) = 1.0   
    case (3)
      ! O3, Turn off O3 mapper continuum for eta=1 case
      xcmapper(4,1) = 0.0
      ! O3, Turn on O3 mappee continuum as contribution
      ! included in the ks
      xcmappee(4,2:neta) = 1.0 
    case (7)
      ! O2, Turn off O2 mapper continuum for eta=1 case 
      xcmapper(5,1) = 0.0
      ! O2, Turn on O2 mappee continuum as contribution
      ! included in the ks      
      xcmappee(5,2:neta) = 1.0      
  end select
  
  select case (ig2)
    case (1)
      ! H2O, Turn off WV continuum for eta=9 case
      xcmapper(1:2,9) = 0.0
      ! H2O mappee continuum treated separately; contribution not
      ! included in the ks.      
      xcmappee(1:2,1:neta-1) = 0.0
    case (2)
      ! CO2, Turn off CO2 mapper continuum for eta=9 case
      xcmapper(3,9) = 0.0
      ! CO2, Turn on CO2 mappee continuum as contribution
      ! included in the ks      
      xcmappee(3,1:neta-1) = 1.0   
    case (3)
      ! O3, Turn off O3 mapper continuum for eta=9 case
      xcmapper(4,9) = 0.0
      ! O3, Turn on O3 mappee continuum as contribution
      ! included in the ks
      xcmappee(4,1:neta-1) = 1.0 
    case (7)
      ! O2, Turn off O2 mapper continuum for eta=1 case 
      xcmapper(5,9) = 0.0
      ! O2, Turn on O2 mappee continuum as contribution
      ! included in the ks  
      xcmappee(5,1:neta-1) = 1.0      
  end select
  
end subroutine setup_continuum

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



