# Chapter exercises

## Combinators

1. λxx.xxx       = Is combinator
2. λxy.xz  	     = Is NOT combinator because of z
3. λxyz.xy(zx)   = Is combinator
4. λxyz.xy(zxy)  = Is combinator
5. λxy.xy(zxy)   = Is NOT combinator because of z

## Normal form or diverge

1. λx.xxx        = This is normal form
2. (λz.zz)(λy.yy)
   (λy.yy)(λy.yy)
   Diverges at this point
3. (λx.xxx)z
   zzz
   This is normal form

## Beta reduce

1. (λabc.cba)zz(λwv.w)
   (λwv.w)zz
   (λv.z)z
   z
2. (λx.λy.xyy)(λa.a)b
   (λy.(λa.a)yy)b
   (λa.a)bb
   bb
3. (λy.y)(λx.xx)(λz.zq)
   (λy.y)(λz.zq)(λz.zq)
   (λy.y)(λz.zq)q
   (λy.y)qq
   qq
4. (λz.z)(λz.zz)(λz.zy)
   (λz.zz)(λz.zy)
   (λz.zy)(λz.zy)
   (λz.zy)y
   yy
5. (λx.λy.xyy)(λy.y)y
   (λx.λy.xyy)y
   λx.xyy
6. (λa.aa)(λb.ba)c
   (λb.ba)(λb.ba)c
   (λb.ba)(a)c
   aac
7. (λxyz.xz(yz))(λx.z)(λx.a)
   (λx.λy.λz.xz(yz))(λx.z)(λx.a)
   (λz1.(λx.z)z1((λx.a)z1))
   (λz1.(λx.z)z1(a))
   (λz1.z(a))
