c                                                                               
c    scale matrix then compute inverse                                          
c     use  svd for matrix inversion                                             
c    Double precision version                                                   
c                                                                               
c    joanne markham oct 1991                                                    
c                                                                               
       subroutine svdinv (n,a,b,det)                                            
       double precision a(10,10),b(10,10),det,w(10),u(10,10),v(10,10),          
     1diag(10),dtemp, tol,wmin,wmax                                                            
c                 
c   move matrix                                                                
c                                                                               
        do 20 i =1,n                                                            
	do 20 j= 1,n                                                                   
20      u(i,j) = a(i,j)
c        write (6,*) ' d ',(diag(i),i=1,n)                                       
c                                                                               
c    for accuracy set diag elements to 1.                                       
c                                                                               
c                                                                               
c   call svd                                                                    
c                                                                               
	call svdcmp ( u, n, n, 10,10,w,v)                                              
c                                                                               
c     check condition number and set small singular values to zero              
c                                                                               
         wmax =0.                                                               
	 wmin = 999999.                                                                
        det = 1.                                                                
	do 50 i=1,n                                                                    
         det = det *w(i)                                                        
	 if (w(i) .gt. wmax)  wmax = w(i)                                              
	 if (w(i) .lt. wmin) wmin = w(i)                                               
50       continue          
	 write (6,*) ' SINGULAR VALUES ', (w(i),i=1,n)
         if (wmin.ne.0.) then                                                   
	 cno = wmax /wmin                                                              
	 write (6,*) ' condition no = ', cno                                           
	 else                                                                          
	 write (6,*)' DET = 0. '                                                       
         endif                                                                  
	 tol =  wmax *1.0d-7                                                           
c                                                                               
c                                                                               
c     compute inverse from u , w and v                                          
c                                                                               
c                                                                               
           do 60 i=1,n                                                          
	       if (w(i) .lt. tol) then                                                 
	       w(i) = 0.                                                               
	       else                                                                    
                                                                                
	       w(i) = 1.0/w(i)                                                         
	       endif                                                                   
60       continue                                                               
                                                                                
 	 do 80 i= 1,n                                                                 
	  do 80 j = 1, n                                                               
          dtemp = 0.                                                            
	  do 70 k = 1,n                                                                
70       dtemp = dtemp + v(i,k)*u(j,k)*w(k)                                     
c        write (6,1002) i, j, dtemp                                             
	 b(i,j) = dtemp                                                                
80        continue	                                                             
1002      format (2i5,f20.6)                                                    

       	return                                                                  
	 end  