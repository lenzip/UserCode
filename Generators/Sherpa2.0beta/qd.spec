### RPM external qd 2.3.13
Source: http://lenzip.web.cern.ch/lenzip/qd-%{realversion}.tar.gz

%prep
%setup -n qd-%{realversion}
./configure --prefix=%i --enable-shared 
# The following hack insures that the bins with the library linked explicitly
# rather than indirectly, as required by the gold linker
%build
make
%install
make install
