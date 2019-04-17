Pre(x0>=0);
x = x0;
z = 0;
while (x != 0) {
  Inv(x>=0 & x+z==x0);
  z = z+1;
  x = x-1;
 }
Post(z==x0);
