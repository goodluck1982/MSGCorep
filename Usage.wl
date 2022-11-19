(* ::Package:: *)


showMSGSeitz::usage="showMSGSeitz[{Rname,v,aU}]  shows the Seitz symbol of an MSG symmetry {Rname,v,aU}. Options: \"format\" can be \"std\""<>
   "(default), \"simple\", or \"TeX\"; \"fullbar\" is True by default; \"antiUcolor\" is Red by default.";
showMSGSym::usage="showMSGSym[listOrSpan]  returns a table of magnetic space group numbers and symbols specified by family number(s). "<>
   "listOrSpan can be a list or a span for family numbers, or even only a family number, such as {20,21,25}, 10;;20, or just 10. "<>
   "listOrSpan can be omitted in which case all 230 families are shown. Both BNS and OG numbers and symbols are given, and the BNS "<>
   "symbols in BC orientation are also given. Default options are \"family\"->\"BNS\", \"color\"->True, and \"BCstyle\"->1. If "<>
   "\"family\"->\"OG\" is specified, listOrSpan represents OG family numbers.";
MSGSeitzTimes::usage="MSGSeitzTimes[brav][{Rname1,v1,antiU1}, {Rname2,v2,antiU2}]    OR\n"<>
                     "MSGSeitzTimes[brav][{Rname1,v1,antiU1}, ..., {Rnamen,vn,antiUn}]\ncalculates the multiplication of two MSG elements "<>
  "{Rname1,v1,antiU1} and {Rname2,v2,antiU2}, or the continuous multiplication of three or more MSG elements. The Bravais lattice brav is needed.";
MSGinvSeitz::usage="MSGinvSeitz[brav][{Rname,v,antiU}]  gives the inversion of the MSG element {Rname,v,antiU}. The Bravais lattice brav is needed.";
MSGpowerSeitz::usage="MSGpowerSeitz[brav][{Rname,v,antiU},n]  calculates {Rname,v,antiU}^n. The Bravais lattice brav is needed.";
DMSGSeitzTimes::usage="DMSGSeitzTimes[brav][{Rname1,v1,antiU1}, {Rname2,v2,antiU2}]    OR\n"<>
                     "DMSGSeitzTimes[brav][{Rname1,v1,antiU1}, ..., {Rnamen,vn,antiUn}]\ncalculates the multiplication of two double MSG elements "<>
  "{Rname1,v1,antiU1} and {Rname2,v2,antiU2}, or the continuous multiplication of three or more double MSG elements. The Bravais lattice brav is needed.";
DMSGinvSeitz::usage="DMSGinvSeitz[brav][{Rname,v,antiU}]  gives the inversion of the double MSG element {Rname,v,antiU}. The Bravais lattice brav is needed.";
DMSGpowerSeitz::usage="DMSGpowerSeitz[brav][{Rname,v,antiU},n]  calculates {Rname,v,antiU}^n for double MSG. The Bravais lattice brav is needed.";
MagRotTimes::usage="MagRotTimes[R1,R2]    OR    MagRotTimes[R1, ..., Rn]\nperforms the multiplication between two rotations R1 and R2 in magnetic point groups "<>
  "or the continuous multiplication of three or more elements";
invMagRot::usage="invMagRot[R]  returns the inverse of a rotation R in magnetic point groups.";
powerMagRot::usage="powerMagRot[R,n]  returns the n-th power of a rotation R in magnetic point groups.";
DMagRotTimes::usage="DMagRotTimes[R1,R2]    OR    DMagRotTimes[R1, ..., Rn]\nperforms the multiplication between two rotations R1 and R2 in double "<>
  "magnetic point groups or the continuous multiplication of three or more elements";
invDMagRot::usage="invDMagRot[R]  returns the inverse of a rotation R in double magnetic point groups.";
powerDMagRot::usage="powerDMagRot[R,n]  returns the n-th power of a rotation R in double magnetic point groups.";
showMagRot::usage="showMagRot[R]  shows the rotation symbol of R for magnetic point groups. Options: \"format\" can be \"std\""<>
   "(default), \"text\", \"simple\", or \"TeX\"; \"fullbar\" is False by default; \"antiUcolor\" is Red by default.";

getMPGElem::usage="getMPGElem[mpg]  gives the list of elements of the specified magnetic point group.";
showMPGinfo::usage="showMPGinfo[range]  shows the information of magnetic point groups. range is optional and is All by default. "<>
   "Default options are \"long\"->True, \"color\"->True, and \"elem\"->False.";
getMPGCorep::usage="getMPGCorep[mpg]  gives the table of coreps for magnetic point group mpg. mpg can be the number or the name "<>
   "string of a magnetic point group, e.g. {5,4}, or \"5.4\", or \"5.4.15\", or \"2/m'\". Full list can be obtained by showMPGinfo[], "<>
   "or by triggering a tip via a wrong input such as getMPGCorep[0]. When the option \"double\"->True is used, the coreps of the "<>
   "corresponding double magnetic point group are given. Default options are \"double\"->False and \"trace\"->False.";
showMPGCorep::usage="showMPGCorep[mpg]  shows the table of corep matrices for magnetic point group mpg in a user-friendly table form. "<>
   "Default options are \"double\"->True, \"rotmat\"->True, \"elem\"->All, \"corep\"->All, \"trace\"->False, \"spin\"->\"downup\", "<>
   "\"cartesian\"->False, and \"linewidth\"->2. For available values of the input argument mpg and the options \"double\", \"elem\", "<>
   "and \"corep\", tips will be triggered by a wrong input, e.g. \"double\"->xx. In this function, \"double\" can be True, False, "<>
   "and Full."; 
MPGCorepDirectProduct::usage="MPGCorepDirectProduct[mpg, coreps1, coreps2]  gives the direct products between coreps1 and "<>
   "coreps2 for magnetic point group mpg. coreps1 and coreps2 are both optional. If coreps2 is omitted, it takes the same value "<>
   "as coreps1, and if both of them are omitted, they both take the vaule All. The output styles 1-4 can be controlled "<>
   "by the option \"output\" with default value 1. For available values of the input arguments mpg, coreps1, and coreps2, "<>
   "and the option \"output\", tips will be triggered by a wrong input, e.g. \"output\"->0. Note that both single-valued "<>
   "and double-valued coreps are calculated.";
showMPGCorepDirectProduct::usage="showMPGCorepDirectProduct[mpg, coreps1, coreps2]  shows the direct products between coreps1 and "<>
   "coreps2 for magnetic point group mpg in a user-friendly table form. coreps1 and coreps2 are both optional. If coreps2 is omitted, "<>
   "it takes the same value as coreps1, and if both of them are omitted, they both take the vaule All. Default options "<>
   "are \"label\"->1 (1 for Mulliken labels and 2 for Gamma labels), \"double\"->True, \"linewidth\"->2, and \"emph\"->None. "<>
   "For available values of the input arguments mpg, coreps1, and coreps2, and the option \"emph\", tips will be triggered "<>
   "by a wrong input, e.g. \"emph\"->0. Note that both single-valued and double-valued coreps are shown by default, and "<>
   "only single-valued coreps are shown when \"double\"->False is used.";

getMSGElem::usage="getMSGElem[{sgno,mno}]  gives the list of elements of the MSG sgno.mno. In fact, only the "<>
  "coset representatives with respect to the translation subgroup are given. When option \"double\"->True is used "<>
  "elements of the double MSG are given (False by default).";
getMLGElem::usage="getMLGElem[brav, MSGelem, kcoord] or getMLGElem[{sgno,mno}, k]\nGives the elements of the magnetic "<>
  "little group (MLG) of k for magnetic space group with number sgno.mno. In the first form, brav is the Bravais "<>
  "lattice, MSGelem is a list of MSG elements, and kcoord is the coordinates of a k-point. In the second form, k can "<>
  "be either the name or coordinates of a k-point, and if the option \"double\"->True is used, the elements of MLG "<>
  "for double MSG is given.";
getMSGType::usage="getMSTType[{sgno,mno}]  gives the type of the MSG sgno.mno, i.e. 1, 2, 3, or 4.";
getMagKStar::usage="getMagKStar[{sgno,mno}, k]  gives the magnetic wave-vector star of k for MSG sgno.mno, and k "<>
  "can be either the name or coordinates of a k-point. If option \"cosets\"->True is used, cosets of the MLG of k "<>
  "are also given.";

MSGSymStd::usage="MSGSymStd[{sgno, mno}]  or  MSGSymStd[sgno]\n"<>
  "The former gives the standard BNS (Belov-Neronova-Smirnova) symbol of the MSG sgno.mno. And the latter gives the standard BNS symbols "<>
  "of all the MSG's with fixed sgno and all available mno's. Default options are \"TeX\"->False and \"BC\"->False."<>
  "\"BC\"->True will give the symbols conforming to the BC orientation.";
MSGSymBNS::usage="MSGSymBNS[{sgno, mno}]  or  MSGSymBNS[sgno]\n"<>
  "The former gives the standard BNS (Belov-Neronova-Smirnova) symbol of the MSG sgno.mno. And the latter gives the standard BNS symbols "<>
  "of all the MSG's with fixed sgno and all available mno's. Default options are \"TeX\"->False and \"BC\"->False."<>
  "\"BC\"->True will give the symbols conforming to the BC orientation.\n"<>
  "In fact, MSGSymBNS is just an alias of MSGSymStd.";
MSGSymBC::usage="MSGSymBC[{sgno, mno}]  or  MSGSymBC[sgno]\n"<>
  "Equivalent to MSGSymStd with \"BC\"->True option.";
MSGSymOG::usage="MSGSymOG[{p,q,r}]  or  MSGSymOG[sgno]\n"<>
  "The former gives the OG (Opechowski-Guccione) symbol of the MSG p.q.r. And the latter gives the OG symbols "<>
  "of all the MSG's with fixed p and all available q.r pairs. Default options are \"TeX\"->False.";

getMLGCorep::usage="getMLGCorep[{sgno,mno}, k] or getMLGCorep[{sgno,num}, k, BZtype]\n"<>
  "Gives the data of the corep of the MLG of k for MSG sgno.mno, and the returned value is an association. k can "<>
  "be either the name or coordinates of a k-point. BZtype is optional and is \"a\" by default. Three options are "<>
  "available and by default they are \"format\"->True, \"abcOrBasVec\"->None and \"trace\"->False. \"abcOrBasVec\" "<>
  "can be used to specify the MSG cell by either the numeric basic vectors or lattice constants such as {a->2, b->3, "<>
  "c->4}, and when \"abcOrBasVec\" is specified (not None) the BZ type will be determined automatically.";
showMLGCorep::usage="showMLGCorep[{sgno,mno}, k] or getMLGCorep[{sgno,num}, k, BZtype]\n"<>
  "Shows the data of the corep of the MLG of k for MSG sgno.mno in a user-friendly table form. k can "<>
  "be either the name or coordinates of a k-point. BZtype is optional and is \"a\" by default. Default options "<>
  "of this function are \"uNumeric\"->False, \"corep\"->All, \"elem\"->All, \"rotmat\"->True, \"trace\"->False, "<>
  "\"spin\"->\"downup\", \"abcOrBasVec\"->None, \"linewidth\"->2, and \"maxDim\"->4.";
getMLGCorepMat::usage="getMLGCorepMat[crinfo,cridx][elmOrList]  get the small corep matrix(es) (or character(s)) "<>
   "of the element or list of elements elmOrList according to crinfo which returned by getMLGCorep. The purpose "<>
   "of this function is to get the corep matrixes for the elements whose translation parts differ from those in "<>
   "getMLGElem by lattice vectors, such as {\"E\",{2,0,1},0}. cridx indicates the index(es) of the requested coreps, "<>
   "such as 2 or {2,3,4} or even 1;;3. cridx is optional and if it is omitted all coreps in crinfo are processed. Default options "<>
   "are \"uNumeric\"->False and \"trace\"->False. For example, getMLGCorepMat[crinfo, \"trace\"->True][{\"E\",{2,0,1},0}] "<>
   "returns the characters (matrix traces) of {\"E\",{2,0,1},0} in all coreps of crinfo.";
getMSGCorep::usage="getMSGCorep[{sgno,mno}, k] or getMSGCorep[{sgno,num}, k, BZtype]\n"<>
  "Gives the data of the full corep of the MSG sgno.mno related to the magnetic wave-vector star of k, and the "<>
  "returned value is an association. k can be either the name or coordinates of a k-point. For simplicity, the "<>
  "same result will be returned no matter which k-point in the same star is given as the input. BZtype is optional "<>
  "and is \"a\" by default. Three options are available and by default they are \"format\"->True, \"abcOrBasVec\""<>
  "->None and \"trace\"->False. \"abcOrBasVec\" can be used to specify the MSG cell by either the numeric basic "<>
  "vectors or lattice constants such as {a->2, b->3, c->4}, and when \"abcOrBasVec\" is specified (not None) the "<>
  "BZ type will be determined automatically.";
showMSGCorep::usage="showMSGCorep[{sgno,mno}, k] or showMSGCorep[{sgno,num}, k, BZtype]\n"<>
  "Shows the data of the full corep of the MSG sgno.mno related to the magnetic wave-vector star of k in a "<>
  "user-friendly talbe form. k can be either the name or coordinates of a k-point. For simplicity, the "<>
  "same result will be returned no matter which k-point in the same star is given as the input. BZtype is optional "<>
  "and is \"a\" by default. Default options of this function are \"uNumeric\"->False, \"corep\"->All, "<>
  "\"elem\"->All, \"rotmat\"->True, \"trace\"->False, \"spin\"->\"downup\", \"abcOrBasVec\"->None, \"linewidth\"->2, "<>
  "and \"maxDim\"->4.";
getMSGCorepMat::usage="getMSGCorepMat[crinfo,cridx][elmOrList]  get the full corep matrix(es) (or character(s)) "<>
   "of the element or list of elements elmOrList according to crinfo which returned by getMSGCorep. The purpose "<>
   "of this function is to get the corep matrixes for the elements whose translation parts differ from those in "<>
   "getMSGElem by lattice vectors, such as {\"E\",{2,0,1},0}. cridx indicates the index(es) of the requested coreps, "<>
   "such as 2 or {2,3,4} or even 1;;3. cridx is optional and if it is omitted all coreps in crinfo are processed. Default options "<>
   "are \"uNumeric\"->False and \"trace\"->False. For example, getMSGCorepMat[crinfo, \"trace\"->True][{\"E\",{2,0,1},0}] "<>
   "returns the characters (matrix traces) of {\"E\",{2,0,1},0} in all coreps of crinfo.";
checkMLGCorep::usage="checkMLGCorep[corepinfo]  checks whether the corepinfo from getMLGCorep satisfies correct "<>
  "multiplications. If the returned numbers are all zeros, the corepinfo goes right. To get accurate multiplications, "<>
  "use \"format\"->False option in getMLGCorep.";
checkMSGCorep::usage="checkMSGCorep[corepinfo]  checks whether the corepinfo from getMSGCorep satisfies correct "<>
  "multiplications. If the returned numbers are all zeros, the corepinfo goes right. To get accurate multiplications, "<>
  "use \"format\"->False option in getMSGCorep.";

MSGCorepDirectProduct::usage="MSGCorepDirectProduct[{sgno,mno}, kin1, kin2]  calculates the decomposition of the "<>
  "direct product of the MSG coreps of kin1 star and kin2 star. The input k-point (kin1 or kin2) can be either "<>
  "its name (only for high-symmetry k-points not k-lines) or its numeric coordinates. Option \"abcOrBasVec\"->None "<>
  "is default, and if None is replaced by the basic vectors or lattice constants, BZ type is determined automatically.";
showMSGCorepDirectProduct::usage="showMSGCorepDirectProduct[{sgno,mno}, kin1, kin2]  shows the decomposition of the "<>
  "direct product of the MSG coreps of kin1 star and kin2 star in a user-friendly table form. The input k-point "<>
  "(kin1 or kin2) can be either its name (only for high-symmetry k-points not k-lines) or its numeric coordinates. "<>
  "Defauly options are \"abcOrBasVec\"->None, \"linewidth\"->2, \"numPerRow\"->4.";

readMagTrace::usage="readMagTrace[filename]  reads the trace.txt file generated by Mvasp2trace. filename is the "<>
  "path to trace.txt file.";
getBandCorep::usage="Three usages:\n"<>
  "getBandCorep[{sgno,mno}, traceData, ikOrListOrSpan, ibOrListOrSpan]\n"<>
  "getBandCorep[{sgno,mno}, traceData, ikOrListOrSpan]\n"<>
  "getBandCorep[{sgno,mno}, traceData]\n"<>
  "This function gives the magnetic-little-group coreps of the Bloch states for the k points specified by "<>
  "ikOrListOrSpan and bands specified by ibOrListOrSpan. If ikOrListOrSpan and ibOrListOrSpan are not specified, "<>
  "coreps of all k points and all bands are given. traceData is the result of readMagTrace. Note that the trace.txt "<>
  "file should be generated using a BC standard cell, or else the traceData may lead to errors of getBandCorep. "<>
  "Default options are \"CompressDegeneracy\"->True, \"showdim\"->True and \"abcOrBasVec\"->None. When numeric basic "<>
  "vectors or lattice constants are specified for \"abcOrBasVec\", the names of some high-symmetry k lines can "<>
  "be identified correctly.";
showBandCorep::usage="showBandCorep[corep, ik, ibOrListOrSpan]  shows the small coreps at the ik-th k-point for the bands "<>
  "specified by ibOrListOrSpan. If ibOrListOrSpan is omitted, all bands at the k-point are shown. corep is the returned "<>
  "value of getBandCorep[{n,m}, tr]. Default option is \"bottomUp\"->True which means that the band energy increases from "<>
  "the bottom up in the table shown."

libMLGCorep::usage="libMLGCorep is an Association read from the libMLGCorep.mx file, which serves as a dictionary of MLG coreps for getBandCorep.";
generateLibMLGCorep::usage="generateLibMLGCorep[]  is used to generate the libMLGCorep.mx file. Note that users "<>
  "need to run this function only when they want to regenerate the libMLGCorep.mx file. And this will take a "<>
  "somewhat long time, e.g. on a 4-core PC, it will take near an hour. By default, the file will be output to the "<>
  "directory Directory[], and this can be changed to a user-defined directory using the \"path\" option";
lookupMLGCorep::usage="lookupMLGCorep[{sgno,num}, kname, kco]  lookups MLG corep in libMLGCorep for quick reference. "<>
  "At present this function is only used in getBandCorep.";

BCTab7d2::usage="BCTab7d2  stores the data in the Tab 7.2 of the BC book with error corrected.";
type1num::usage="type1num[sgno]  gives the number mno for type-I MSG sgno.mno.";
type2num::usage="type2num[sgno]  gives the number mno for type-II MSG sgno.mno.";
type3nums::usage="type3num[sgno]  gives the list of available mno's for type-III MSG sgno.mno.";
type4nums::usage="type4num[sgno]  gives the list of available mno's for type-IV MSG sgno.mno.";
index2subSG::usage="index2subSG[sgno]  gives the maximal non-isomorphic subgroups of type I (translationengleiche "<>
  "or isotranslational) and of index 2 for SG sgno.";
type3USubInfo::usage="type3USubInfo  stores the information of the unitary subgroup of type-III MSGs.";
type3kpairDict::usage="type3kpairDict  stores the information of pairing of k-points for given type-III "<>
  "MSGs and their subgroups.";
type4AUTrans::usage="type4AUTrans[brav,BWlatt]  gives the anti-unitary translation of brav and BWlatt for "<>
  "type-IV MSGs. e.g. brav=\"OrthPrim\", BWlatt=\"P_B\"";
MSGtype::usage="MSGtype[{sgno,mno}]  gives the type 1, 2, 3, or 4 of the MSG sgno.mno.";
type1MSGSymText::usage="type1MSGSymText  stores the text-version symbols for type-I MSGs.";
type2MSGSymText::usage="type2MSGSymText  stores the text-version symbols for type-II MSGs.";
type3MSGSymText::usage="type3MSGSymText  stores the text-version symbols for type-III MSGs.";
type4MSGSymText::usage="type4MSGSymText  stores the text-version symbols for type-IV MSGs.";
MSGSymText::usage="MSGSymText  stores the text-version symbols for all MSGs.";
type1MSGSymTextBC::usage="type1MSGSymTextBC  stores the text-version symbols conforming to the BC orientation "<>
  "for type-I MSGs.";
type2MSGSymTextBC::usage="type2MSGSymTextBC  stores the text-version symbols conforming to the BC orientation "<>
  "for type-II MSGs.";
type3MSGSymTextBC::usage="type3MSGSymTextBC  stores the text-version symbols conforming to the BC orientation "<>
  "for type-III MSGs.";
type4MSGSymTextBC::usage="type4MSGSymTextBC  stores the text-version symbols conforming to the BC orientation "<>
  "for type-IV MSGs.";
MSGSymTextBC::usage="MSGSymTextBC  stores the text-version symbols conforming to the BC orientation "<>
  "for all MSGs.";
MSGSymTextOG::usage="MSGSymTextOG  stores the text-version OG (Opechowski-Guccione) symbols of MSGs."
OGtoBNS::usage="OGtoBNS[{p,q,r}]  converts the OG number {p,q,r} of a MSG to its BNS number {n,m}."
BNStoOG::usage="BNStoOG[{n,m}]  converts the BNS number {n,m} of a MSG to its OG number {p,q,r}."
MPGinfo::usage="MPGinfo  stores the information of magnetic point groups.";
