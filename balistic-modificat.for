      write(*,19)
   19 format(10x,'Choose the initial velocity')
      read(*,*) V0
      write(*,21)
   21 format(10x,'Choose a planet: 1-Mercury, 2-Venus, 3-Earth, 4-Mars, 5-Jupiter, 6-Saturn, 7-Uranus, 8-Neptune')
      read(*,*) m
      open(1,file='planets.dat',status='old')
      read(1,*) (g,i=1,m)
      write(*,1)
    1 format(10x,'Difficulty level=?')
      read(*,*) l
      D=40
      Dx=15
      Dy=7
      Xmax=V0**2/g
      Ymax=V0**2/(2*g)
      Xt=Xmax/2
      Yt=Ymax/2.5
      k=1
   12 write(*,2) k
    2 format(25x,'The',I3,'th trial')
      write(*,6) Xmax,Xt,Yt
    6 format(3x,'Xmax=',F10.4,10x,'Xt=',F10.5,10x,'Yt=',F10.5) 
      if(Xt.gt.Xmax) goto 15
      write(*,3)
    3 format(5x,'Theta=?')
      read(*,*) T
      T=T*3.1415/180
      Yk=Xt*(TAN(T)-g*Xt/2/((V0*COS(T))**2))
      Ytp=Yt+D/2
      Ytm=Yt-D/2
      if(Yk.gt.Ytp) goto 7
      if(Yk.lt.Ytm) goto 8
      write(*,9)
    9 format(10x,'****Congratulations!!!****',10x,'****The target was destroyed****')
      goto 10
    7 write(*,4)
    4 format(35x,'The target is below the projectile')
      goto 20
    8 write(*,5)
    5 format(35x,'The target is above the projectile')
   20 if(l.eq.1) goto 11
      Xt=Xt+k*Dx
      if(l.eq.2) goto 11
      Yt=Yt+(k-1)*Dy
      write(*,14)
   14 format(35x,'The target is going out from the protected region!')
      if(k.gt.10) goto 17
      goto 11
   17 write(*,13)
   13 format(1x,'Maybe the target is moving too fast...Do you want to decrease its speed by 25%? Press 1 for YES or 0 for NO')
      read(*,*)f
      if(f.eq.1) goto 18
      goto 11
   18 Dx=Dx-Dx/4
      Dy=Dy-Dy/4
   11 k=k+1
      goto 12
   15 write(*,16)
   16 format(25x,'The target escapes from the protected region!')
   10 stop
      end



