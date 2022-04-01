/* REXX */ 
b1=bitarray('Create',100000)                                           
do i=99000 to 99016                                                    
   call bitarray('set',b1,i); call bitarray('Dump',b1,i) ; test=1      
   say 'After 'bitarray('get',b1,i)                                    
end                                                                    
do while test=1 ; test=0;           end                                
do until test=1 ;         test=1;end                                   
do i=99000 to 99016                                                    
   call bitarray('set',b1,i,0)                                         
   call bitarray('Dump',b1,i)                                          
   say 'After 'bitarray('get',b1,i)                                    
   if test=1 then test=0                                               
end                                                                    
exit    
