# spextool
[Spextool v4.1](http://irtfweb.ifa.hawaii.edu/~spex/observer/) modified to be compatible with P200/TripleSpec data.

## Usage

1. Install IDL.
2. Download this library.
3. Unzip.
4. Move to anywhere you want. 
5. Open IDL->Window->Preferences->IDL->Paths.
6. Add the path to this library.
4. Also add the paths to other libraries (i.e., astron, coyote, and mpfit).

## Tips in the original README

1. Spextool Users:  Please read the Spextool manual found in /Spextool/manuals for instructions on how to install and use Spextool.
2. Xtellcor_general Users:  Please read the xtellcor_general manual found in /Spextool/manuals for instructions on how to install and use Spextool.

## Notes

- 2023-01-04: `other/misc/polyfillaa.pro` called by `pro/mc_extspec2d.pro` attempts to compile `other/misc/polyclip.c`, which is a C version of the Sutherland Hodgemand algorithm. This version is ~50x faster than the IDL-native method. For Windows, MSVC is needed to allow this. See [How to create a 64-bit C DLL with MSVC 2017 and IDL 8.7 for Windows 10](https://www.l3harrisgeospatial.com/Support/Maintenance-Detail/ArtMID/13350/ArticleID/23647/How-to-create-a-64-bit-C-DLL-with-MSVC-2017-and-IDL-87-for-Windows-10) for a detailed instruction on how to configure a compilation environment. Note that MSVC 2017 is not the only version that works. MSVC 2022 also works well.
- 2023-01-03: `instruments\paltspec\pro\mc_paltspecohcals.pro` was modified to create a pure sky frame using (A+B) - abs(A-B) for the 2-image-case in the wavelength calibration.
- 2023-01-02: `pro/xspextool.pro` was modified to be compatible with the structure returned by `pro/mc_extspec2d.pro` for P200/TripleSpec data.
- 2022-12-25: `pro/mc_rdwavecal2d.pro` was modified to load wavecal file properly and to ignore the unavailable keywords `DISPO*` for P200/TripleSpec data.  