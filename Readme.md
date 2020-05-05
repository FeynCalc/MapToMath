# MapToMath

[![license: MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://github.com/FeynCalc/maptomath/blob/master/LICENSE)

`MapToMath` is a Mathematica package for importing Maple expressions (as strings) into Mathematica.

It is based on the code of `MapleConverter 0.95` developed by Chris Willett and Lars Hohmuth. MapleConverter is available from [Wolfram Library Archive](https://library.wolfram.com/infocenter/Demos/188) under the MIT license.
We kindly acknowledge Lars Hohmuth and Wolfram Research, Inc for relicensing the original code under the MIT
license in 2020. 

`MapToMath` is still in an early development stage and is not meant for productive use.


# Usage

```
<<MapToMath`
```

```
MTMConvert["ln(x)^5"]
```

```
MTMConvert["(-2*zeta[2]+1/2*Pi^2)/ep-3/8*I*Pi^3-4*zeta[2]+Pi^2+2*zeta[\
3]+9/4*I*Pi*zeta[2]+(
 -3/2*I*Pi^3*ln(2)+2*Pi^2-27/16*Pi^2*zeta[2]+9*I*Pi*ln(2)*zeta[2]+9/2*\
I*Pi*zeta[2]+21/32*Pi^4-3/4*I*Pi^3-109/10*zeta[2]^2-8*zeta[2]+4*zeta[\
3])*ep"]
```

```
MTMConvert["arcsin(x)/y"]
```

```
MTMConvert["GAMMA(x)*GAMMA(y)/GAMMA(y+x)"]
```

# License

MapToMath is covered by the MIT license.

Copyright 2020 Vladyslav Shtabovenko

Copyright 2020 Wolfram Research, Inc  

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN 
THE SOFTWARE.
