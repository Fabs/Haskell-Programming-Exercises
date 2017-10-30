1. Will return 5 because the lenght of 5 is greater than 3 sized list

   λ max (length [1,2,3]) (length [8,9,10,11,12])
   5

2. Will return LT because 12 is less than 15

   λ compare (3*4) (3*5)
   LT

3. Will not compile. String and Bool are different types and compare needs two of the same types that are a member of Eq

   λ compare "Julie" True
   
   <interactive>:3:17: error:
       • Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
       • In the second argument of ‘compare’, namely ‘True’
         In the expression: compare "Julie" True
         In an equation for ‘it’: it = compare "Julie" True
   Prelude

4. Will return False because 15 is less than 18.

   λ (5+3) > (3+6)
   False
