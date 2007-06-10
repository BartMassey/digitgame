# Copyright (C) 2007 Bart Massey
# ALL RIGHTS RESERVED
# Please see the end of this file for license information.

HC = ghc
HCFLAGS = -O2 # -prof -auto-all
TARGETS = digitgame
SRCS = digitgame.hs

all: $(TARGETS)

digitgame: $(SRCS)
	$(HC) $(HCFLAGS) --make -o digitgame digitgame.hs

sinewave: sinewave.hs $(SRCS)
	$(HC) $(HCFLAGS) --make -o sinewave sinewave.hs

readtest: readtest.hs $(SRCS)
	$(HC) $(HCFLAGS) --make -o readtest readtest.hs

clean:
	rm -f *.o *.hi *.hp *.prof $(TARGETS)

# Redistribution and use in source and binary forms, with or
# without modification, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above
#       copyright notice, this list of conditions and the following
#       disclaimer.
#     * Redistributions in binary form must reproduce the
#       above copyright notice, this list of conditions and the
#       following disclaimer in the documentation and/or other
#       materials provided with the distribution.
#     * Neither the name of Bart Massey, nor the names
#       of other affiliated organizations, nor the names
#       of other contributors may be used to endorse or promote
#       products derived from this software without specific prior
#       written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.