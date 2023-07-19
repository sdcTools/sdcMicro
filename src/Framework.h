#ifndef __FRAMEWORK_H
#define __FRAMEWORK_H

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

#ifdef _MSC_VER
	#include <io.h>
	#include <sys\types.h>
	#include <sys\stat.h>
#else
	#include <strings.h>
	#include <stdint.h>
	#include <unistd.h>
	#include <sys/types.h>
	#include <sys/stat.h>
	enum
	{
		O_BINARY  = 0
	};

	#include <time.h>
	#include <sys/time.h>

	#define _open	open
	#define _close	close
	#define _lseek	lseek
	#define _read	read
	#define _write	write
#endif

#include <fcntl.h>

#define	TRUE				1
#define	FALSE				0

#define ZSA 0

typedef void (* VoidFunctionVoid)(void);
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned long ulong;
typedef unsigned long DWORD;
typedef unsigned char uchar;
typedef int BOOL;

// ============================= Display Messages =================================
extern char g_TxtBuffer[1024];						// character TxtBufferfer to display messages
int OS_Printf(const char *Str, ...);
BOOL ShowProgression(const char *Text, int CurrentIndex, int MaxIndex, int &Progression, int Step = 20);

// ============================= Assert =================================
#ifdef _DEBUG
	//#ifndef ASSERT
		#define ASSERT(n) { if (!(n)) { OS_Printf("assert: %s %d => %s\n", __FILE__, __LINE__, #n); } /*assert(n);*/ }
	//#endif
#else
	#ifndef ASSERT
		#define ASSERT(n)
	#endif
#endif

// ============================= Max, Min, Swap =================================
template <class T>
inline T Max(T a, T b)
{
	return ( a >= b ? a : b );
}

template<class T>
inline T Min(T a, T b)
{
	return ( a <= b ? a : b );
}

template<class T>
inline T Abs(T a)
{
	return (a >= (T)0 ? a : (T)-a);
}

template<class T>
inline void Swap(T &a,T &b)
{
	T Tmp;

	Tmp = a;
	a = b;
	b = Tmp;
}

// ============================= CChainedList =================================
class CChainedList								// Chained list management
{
public:
	CChainedList *m_pPrev, *m_pNext; 	 	// Ptr to next and previous element

public:
	CChainedList(void)
	{
		m_pPrev = m_pNext = NULL;
	}

	virtual ~CChainedList() { Remove(); }	// Destroyer remove element from list

	void AddAfter(CChainedList *Ptr)			// add element just after *Ptr
	{
		if (Ptr->m_pNext)
			Ptr->m_pNext->m_pPrev = this;
		m_pNext = Ptr->m_pNext;
		m_pPrev = Ptr;
		Ptr->m_pNext = this;
	}

	void AddEnd(CChainedList *Ptr)			// add element at the end of *Ptr list
	{
		while (Ptr->m_pNext)
			Ptr = Ptr->m_pNext;

		Ptr->m_pNext = this;
		m_pPrev = Ptr;
		m_pNext = NULL;
	}

	void AddBefore(CChainedList *Ptr)		// add element just before *Ptr
	{
		if (Ptr->m_pPrev)
			Ptr->m_pPrev->m_pNext = this;
		m_pNext = Ptr;
		m_pPrev = Ptr->m_pPrev;
		Ptr->m_pPrev = this;
	}

	void Remove(void)								// remove element from list
	{
		if (m_pPrev)
			m_pPrev->m_pNext = m_pNext;

		if (m_pNext)
			m_pNext->m_pPrev = m_pPrev;

		m_pPrev = NULL;
		m_pNext = NULL;
	}

	void DeleteAllNext(void)
	{
		CChainedList *Ptr = m_pNext;

		while (m_pNext)
		{
			CChainedList *Tmp = Ptr->m_pNext;
			delete Ptr;
			Ptr = Tmp;
		}
	}

	void DeleteAll(void)
	{
		CChainedList *Ptr = this;

		while (Ptr->m_pPrev)
			Ptr = Ptr->m_pPrev;

		Ptr->DeleteAllNext();
		delete Ptr;
	}
};

// ============================= Bounding =================================
template <class T, class U>
inline U Bound(U v, T Min, T Max)
// return v bounded by [Min, Max]
{
	if (v < Min)
		return (U) Min;
	if (v > Max)
		return (U) Max;
	return (U) v;
}

template <class T>
inline T Modulo(T n, T Div)
{
	if (Div <= 0)
		return 0;

	while (n >= Div)
		n -= Div;
	while (n < 0)
		n += Div;

	return n;
}

template <class T>
inline T SignedModulo(T n, T Div)
{
	while (n >= Div * 0.5)
		n -= Div;
	while (n < -Div * 0.5)
		n += Div;

	return n;
}

template <class T>
inline T Squared(T n)
{
	return n * n;
}


//============================================= Macro & additional function

#define	ForLoop(x, NbStep)	for (x = 0; x < NbStep; ++x)
#define	ForLoopD(x, NbStep)	for (int x = 0; x < NbStep; ++x)

#define ClearMem(x)				memset(&x, 0, sizeof(x))
#define ClearMemV(x, v)			memset(&x, v, sizeof(x))
#define ClearMemT(x, n)			memset(x, 0, sizeof(x[0]) * (n))
#define ClearMemVT(x, v, n)	memset(x, v, sizeof((x)[0]) * (n))
#define CleanDelete(x)  { if (x) delete x; x = NULL; }
#define CleanDeleteT(x)  { if (x) delete[] x; x = NULL; }

#define CleanRelease(x)	 								\
		{						 								\
			if (x)			 								\
			{					 								\
				x->Release();								\
				x = NULL;	 								\
			}					 								\
		}

extern int g_NbNew;

//============================================= Strings

#ifdef _MSC_VER
	#define stricmp _stricmp
	#define strnicmp _strnicmp
#else
	#if !defined(stricmp)
	//	int stricmp(char *str1, char *str2);
		#define stricmp strcasecmp
	#endif

	#if !defined(strnicmp)
		int strnicmp(char *str1, const char *str2, int n);
	#endif
#endif // _MSC_VER

char *Strncpy(char *Dst, const char *Src, int Max, BOOL Warn = TRUE);
char *ReplaceChar(char *Str, char OldChar, char NewChar);
char *Stristr(char *Ptr, char *SubString, BOOL LeaveAfter = FALSE, BOOL ReturnNULL = TRUE);

	//=== Parsing
char *RemoveComment(char *Ptr, int Size = -1);	// remove text between /* & */
char *GoToNextLine(char *Ptr);						// renvoie Ptr avanc� jusqu'apr�s le '\n' suivant
char *GoTo1stChar(char *Ptr);
char *ParseString(char *Ptr, char *Str, int Size, BOOL AdvanceTo1stChar = TRUE);
char *ParseLine(char *Ptr, char *Str, int Size, BOOL AdvanceTo1stChar = TRUE);

//============================================= Time function

#if defined _MSC_VER

struct timezone
{
  int  tz_minuteswest; /* minutes W of Greenwich */
  int  tz_dsttime;     /* type of dst correction */
};

struct timeval
{
        long    tv_sec;         /* seconds */
        long    tv_usec;        /* and microseconds */
};


int gettimeofday(struct timeval *tv, struct timezone *tz);

#endif // _MSC_VER

uint TimeGetMilliSecond(void);

// ============================= CTooFile =============================
class CTooFile
{
public:
	static BOOL m_TextMode;	//To save String without the final '\0', when use Operator <<

	int m_Pos, m_FileSize, m_Hd;
	uchar *m_pMemory;

	//CTooFile(void) { Pos = 0; Memory = NULL; FileSize = 0; };

public:
	static CTooFile *Open(const char *Name, BOOL Warn = TRUE);
	static CTooFile *Create(const char *Name, int Mode = O_RDWR | O_BINARY);
	int Read(void *dst, long Nb);			// Read Nb chars from File to dst;
			// return number of really read chars (if end of File occurs before read Nb chars)
	int Seek(int Off, int Mode);			// Seek File; Mode = SEEK_SET, SEEK_CUR or SEEK_END; return current pos
	int Tell(void);							// return current pos
	void Close(void);							// Close File
	int Size(void);							// return File size
	int GetPos(void);							// return current pos (faster than Tell() )
	int Write(const void *Src, int Nb);	// Write Nb chars from Src to File
};

	// ==== Stream In (Read)
template<class T> inline CTooFile & operator >> (CTooFile &Fp, T &t)
{
	Fp.Read(&t, sizeof(t));
	return Fp;
}

inline char *PutName(char *Path, const char *Name)
// Put Name to the path: PutName("Tmp\\", "Toto.Txt") => "Tmp\\Toto.Txt"
{
	int i = 0, j = 0;
	while (Path[i] != 0)
	{
		if (Path[i] == '\\' || Path[i] == '/')
			j = i+1;
		++i;
	}

	strcpy(Path+j, Name);
	return Path;
}

inline char *GetName(char *Path)
// Get filename without path: GetName("Tmp\\Toto.Txt") => "Toto.Txt"
{
	int i = 0, j = 0;
	while (Path[i] != 0)
	{
		if (Path[i] == '\\' || Path[i] == '/')
			j = i+1;
		++i;
	}

	return Path+j;
}

char *PutExt(char *Name, const char *Ext);
		// put the Ext to the Name: PutExt("Toto.Tmp", ".Txt") => "Toto.Txt"
char *GetExt(char *Name);
		// Get Ext from a filename: GetExt(Toto.Txt) => ".Txt"

// ============================= Stream Out (Write) ===========================
template<class T> inline CTooFile & operator << (CTooFile &Fp, T &t)
{
	Fp.Write(&t, sizeof(t));
	return Fp;
}

inline CTooFile & operator << (CTooFile &Fp, const char *t)
// write char string
{
	uint s = (uint) strlen(t);

//	if (!CTooFile::m_TextMode)
//		++s;

	Fp.Write(t, s);

	return Fp;
}


// ============================= CRandom ===========================
class CRandom
{
public:
	static int m_q1, m_q2, m_q3, m_p1, m_p2, m_p3;
	static uint m_mask1, m_mask2, m_mask3;
	static int m_shft1, m_shft2, m_shft3;

	// Combination of 3 tausworth generators -- assumes 32-bit integers
	uint m_s1, m_s2, m_s3;					// The seeds
	uint m_bs1, m_bs2, m_bs3;  			// backup of The seeds
	int m_Call;

public:
	CRandom(void) { m_s1 = 390451501, m_s2 = 613566701, m_s3 = 858993401; m_Call = 0; }

	float Get(void);												// Return a number in [0.0; 1.0[
	int Get(int NumMax) { return (int) (Get() * NumMax); }		// Return a int in [0; NumMax[
	void SetSeed(uint s1 = 390451501, uint s2 = 613566701, uint s3 = 858993401);
	void RestoreSeed(void);
	void SaveSeed(void);
};

extern CRandom g_TooRandom;


//float RandomS(void);
inline float Random(void)						// Return a float in [0.0; 1.0[
{
	return g_TooRandom.Get();
}

inline int Random(int NumMax)					// Return a int in [0; NumMax[
{
	return g_TooRandom.Get(NumMax);
	//float r = g_TooRandom.Get();
	//return r * NumMax;
}

inline void SetRandSeed(uint s1 = 390451501, uint s2 = 613566701, uint s3 = 858993401)
{
	g_TooRandom.SetSeed(s1, s2, s3);
}

inline void RestoreRandSeed(void)
{
	g_TooRandom.RestoreSeed();
}

inline void SaveRandSeed(void)
{
	g_TooRandom.SaveSeed();
}

// ========================= Sum ==========================

template<class T>
T Sum(T *pArray, int NbElement)
{
	T Sum = 0;

	ForLoopD (i, NbElement)
		Sum += pArray[i];

	return Sum;
}

// ========================= Matrix ==========================

template <class T>
class CMatrix			// Row x Column ; Access by [RowNum][ColumnNum]
{
public:
	int m_NbRow, m_NbColumn;
	T *m_pData;

public:
	CMatrix(int NbRow = 0, int NbColumn = 0)
	{
		m_pData = NULL;
		m_NbColumn = m_NbRow = 0;

		if (NbRow)
			Init(NbRow, NbColumn);
	}

	void Init(int NbRow, int NbColumn)
	{
		Uninit();
		m_NbRow = NbRow;
		m_NbColumn = NbColumn;
		m_pData = new T[NbRow * NbColumn];
	}

	void Uninit(void) { CleanDeleteT(m_pData); }

	~CMatrix() { Uninit(); }

   T * operator [] (int Index) { ASSERT((uint)Index < (uint) m_NbRow); return m_pData + Index * m_NbColumn; }

	void Clear(void)
	{
		ForLoopD (i, m_NbRow)
		{
			ForLoopD (j, m_NbColumn)
				(*this)[i][j] = (T) 0.0;
		}
	}

	void Print(const char *Name = NULL)
	{
		if (Name)
			OS_Printf("%s %dx%d:\n", Name, m_NbRow, m_NbColumn);

		ForLoopD (i, m_NbRow)
		{
			OS_Printf("%2d:", i);

			ForLoopD (j, m_NbColumn)
				OS_Printf(" %g", (double)((*this)[i][j]));

			OS_Printf("\n");
		}
	}

	void Transpose(void)
	{
		if (m_NbRow == m_NbColumn)
		{
			for (int i = 1; i < m_NbRow; ++i)
			{
				ForLoopD (j, i)
					Swap((*this)[i][j], (*this)[j][i]);
			}
		}
		else
		{
			CMatrix<T> Tmp(m_NbColumn, m_NbRow);

			ForLoopD (i, m_NbRow)
			{
				ForLoopD (j, m_NbColumn)
					Tmp[j][i] = (*this)[i][j];
			}

			Swap(m_pData, Tmp.m_pData);
			Swap(m_NbRow, m_NbColumn);
		}
	}
};

template <class T>
inline CMatrix<T> *MatrixMultiply(CMatrix<T> &MatA, CMatrix<T>&MatB)
{
	ASSERT(MatA.m_NbColumn == MatB.m_NbRow);

	CMatrix<T> *pOutMat = new CMatrix<T>(MatA.m_NbRow, MatB.m_NbColumn);

	ForLoopD (i, MatA.m_NbRow)
	{
		ForLoopD (j, MatB.m_NbColumn)
		{
			(*pOutMat)[i][j] = (T) 0.0;

			ForLoopD (k, MatA.m_NbColumn)
				(*pOutMat)[i][j] += MatA[i][k] * MatB[k][j];
		}
	}

	return pOutMat;
}

template <class T>
inline CMatrix<T> *MatrixTranspose(CMatrix<T> &Mat)
{
	CMatrix<T> *pTranspose = new CMatrix<T>(Mat.m_NbColumn, Mat.m_NbRow);

	ForLoopD (i, Mat.m_NbRow)
	{
		ForLoopD (j, Mat.m_NbColumn)
			(*pTranspose)[j][i] = Mat[i][j];
	}

	return pTranspose;
}


// ========================= Main Macro ==========================

typedef int (* MainFunction)(int argc, char *argv[]);
extern MainFunction g_MyMain;
extern BOOL g_InMain;

inline int SubMain(int argc, char *argv[])
{
	int Result = -1;

	g_InMain = TRUE;

	try
	{
		Result = g_MyMain(argc, argv);
	}
	catch(int )//ErrorCode)
	{
	}

	if (g_NbNew)
		OS_Printf("\n===> Error: %d New not Deleted ..!!\n\n", g_NbNew);

	g_InMain = FALSE;

	return Result;
}

#ifdef __PLUGIN
	#define SystemMain(MyMain)		\
		DS_DLL stata_call(int argc, char *argv[])	{	g_MyMain = MyMain; return SubMain(argc, argv);	}
#else
	#define SystemMain(MyMain)		\
		int main(int argc, char *argv[])	{	g_MyMain = MyMain; return SubMain(argc - 1, argv + 1);	}
#endif	// __PLUGIN

#endif // __FRAMEWORK_H

/**
 * Framework : contains basic multi-purpose functions & classes
 *
 * copyright: Organisation For Economic Co-Operation And Development
 *
 * 	This program is free software; you can redistribute it and/or modify it under the terms of the
 * 	GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 	2.1 of the License, or (at your option) any later version.
 *
 * 	This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * 	without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 	See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */


// ============================= Display Messages =================================
#if !defined(__PLUGIN)
	#include <iostream>
#endif

char g_TxtBuffer[1024];						// character TxtBufferfer to display messages

int OS_Printf(const char *Str, ...)
{
	va_list ArgPtr;

	va_start(ArgPtr, Str);
	va_end(ArgPtr);

#ifdef __PLUGIN
	int Ret;
	//if (Stristr(g_TxtBuffer, "Error") || Stristr(g_TxtBuffer, "Warning"))
	//	Ret = g_pDsInterface->PrintErr(g_TxtBuffer);
	//else
		Ret = g_pDsInterface->PrintStd(g_TxtBuffer);
//	g_pDsInterface->poll();
	g_pDsInterface->FlushStdOut();
	g_pDsInterface->PollNow();		// Flush Output

	return Ret;
#else
	return 0;
#endif
}


BOOL ShowProgression(const char *Text, int CurrentIndex, int MaxIndex, int &Progression, int Step)
{
	int Percent = (CurrentIndex + 1) * 100 / MaxIndex;
	BOOL Ret = FALSE;

	if (Percent >= Progression + Step)
	{
		if (!Progression)
			OS_Printf("%s:", Text);

		Progression = Percent;
		OS_Printf(" ...%d%%", Progression);
		Ret = TRUE;
	}

	if (Percent >= 100)
	{
		OS_Printf("\n");
		Ret = FALSE;
	}

	return Ret;
}

///============================================= Strings
#if !defined(_MSC_VER) && !defined(strnicmp)
	int strnicmp(char *str1, const char *str2, int n)
	{
		for (int i = 0; i < n; ++i)
		{
			char c1 = str1[i], c2 = str2[i];

			if (c1 >= 'a' && c1 <= 'z')
				c1 += 'A' - 'a';

			if (c2 >= 'a' && c2 <= 'z')
				c2 += 'A' - 'a';

			int d = c1 - c2;

			if (!d)
			{
				if (!str1[i])
					break;
				continue;
			}

			return d;
		}

		return 0;
	}
#endif // _MSC_VER

#if !defined(_MSC_VER) && !defined(stricmp)

int stricmp(char *str1, char *str2)
{
	for (int i = 0;; ++i)
	{
		char c1 = str1[i], c2 = str2[i];

		if (c1 >= 'a' && c1 <= 'z')
			c1 += 'A' - 'a';

		if (c2 >= 'a' && c2 <= 'z')
			c2 += 'A' - 'a';

		int d = c1 - c2;

		if (!d)
		{
			if (!str1[i])
				return 0;

			continue;
		}

		return d;
	}
}
#endif // _MSC_VER

char *Strncpy(char *Dst, const char *Src, int Max, BOOL Warn)
{
	if (Max > 0)
	{
		strncpy(Dst, Src, Max - 1);
		Dst[Max-1] = 0;
	}
	return Dst;
}

char *ReplaceChar(char *Str, char OldChar, char NewChar)
{
	char *Ret = Str;

	while (*Str)
	{
		if (*Str == OldChar)
			*Str = NewChar;

		++Str;
	}

	return Ret;
}

char *Stristr(char *Ptr, char *SubString, BOOL LeaveAfter, BOOL ReturnNULL)
{
	int l = (int) strlen(SubString);

	while (*Ptr)
	{
		if (!strnicmp(SubString, Ptr, l))
		{
			if (LeaveAfter)
				Ptr += strlen(SubString);
			return Ptr;
		}

		++Ptr;
	}

	if (ReturnNULL)
		return NULL;
	return Ptr;
}

///============================================= Parsing
char *RemoveComment(char *Ptr, int Size)
{
	if (Size < 0)
		Size = (int) strlen(Ptr) + 1;

	for (int n = 0; *Ptr; ++n)
	{
		if (Ptr[0] == '/' && Ptr[1] == '*')
		{
			char *Dst = Ptr;
			BOOL Found = FALSE;

			Ptr += 2;
			n += 2;

			while (*Ptr)
			{
				if (Ptr[0] == '*' && Ptr[1] == '/')
				{
					Ptr += 2;
					n += 2;
					Found = TRUE;
					break;
				}

				++Ptr;
				++n;
			}

			if (!Found)
			{
				break;
			}

			int Delta = Size - n;

			Size -= Delta;
			n -= Delta;
			memcpy(Dst, Ptr, Delta);
			Ptr = Dst;
			if (!*Ptr)
				break;
		}

		++Ptr;
	}

	return Ptr;
}


char *GoToNextLine(char *Ptr)
{
	ASSERT(Ptr != NULL);

	while (*Ptr != '\n' && *Ptr != '\r' && *Ptr != 0)
		++Ptr;

	if (*Ptr == 0)
		return Ptr;

	if (*Ptr == '\n')
	{
		if (Ptr[1] == '\r')
			return Ptr + 2;

		return Ptr + 1;
	}

	if (Ptr[1] == '\n')
		return Ptr + 2;

	return Ptr + 1;
}


char *GoTo1stChar(char *Ptr)
{
	while ((*Ptr == ' ' || *Ptr == '\t') && *Ptr != 0 && *Ptr != '\r' && *Ptr != '\n')
		++Ptr;

	return Ptr;
}


char *ParseString(char *Ptr, char *Str, int Size, BOOL AdvanceTo1stChar)
{
	//BOOL Warn = FALSE;
	int i = 0;

	if (AdvanceTo1stChar)
		Ptr = GoTo1stChar(Ptr);

	BOOL MultiString = (*Ptr == '"');

	if (MultiString)
		++Ptr;

	--Size;

	while (1)
	{
		if (MultiString)
		{
			if (*Ptr == '"' || *Ptr == 0 || *Ptr == '\n' || *Ptr == '\r')
			{
				if (*Ptr == '"')
					++Ptr;
				break;
			}
		}
		else
		{
			if (!(*Ptr != ' ' && *Ptr != 0 && *Ptr != '\n' && *Ptr != '\r' && *Ptr != '\t'))
				break;
		}

		if (Ptr[0] == '/' && Ptr[1] == '/' && (i == 0 || Ptr[-1] != ':'))
			break;

		if (i >= Size)
		{
			//Warn = TRUE;
			break;
		}

		if (Ptr[0] == '\\')
			Str[i++] = '/';
		else
			Str[i++] = *Ptr;

		++Ptr;
	}

	Str[i] = 0;
	return Ptr;
}


char *ParseLine(char *Ptr, char *Str, int Size, BOOL AdvanceTo1stChar)
{
	//BOOL Warn = FALSE;
	int i = 0;

	if (AdvanceTo1stChar)
		Ptr = GoTo1stChar(Ptr);

	--Size;

	while (1)
	{
		if (!(*Ptr != 0 && *Ptr != '\n' && *Ptr != '\r'))
			break;

		if (Ptr[0] == '/' && Ptr[1] == '/' && (i == 0 || Ptr[-1] != ':'))
			break;

		if (i >= Size)
		{
			//Warn = TRUE;
			break;
		}

		if (Ptr[0] == '\\')
			Str[i++] = '/';
		else
			Str[i++] = *Ptr;

		++Ptr;
	}

	Str[i] = 0;
	return Ptr;
}


//============================================= Time function
#if defined _MSC_VER

#include <time.h>

#if defined(_MSC_VER) || defined(_MSC_EXTENSIONS)
	#define DELTA_EPOCH_IN_MICROSECS 11644473600000000Ui64
#else
	#define DELTA_EPOCH_IN_MICROSECS 11644473600000000ULL
#endif

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <Windows.h>
#include <Winbase.h>

/*typedef struct _FILETIME
{
	DWORD dwLowDateTime;  DWORD dwHighDateTime;
} FILETIME, *PFILETIME;*/

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
	FILETIME ft;
	unsigned __int64 tmpres = 0;
	static int tzflag;

	if (tv != NULL)
	{
		GetSystemTimeAsFileTime(&ft);

		tmpres |= ft.dwHighDateTime;
		tmpres <<= 32;
		tmpres |= ft.dwLowDateTime;

		/*converting file time to unix epoch*/
		tmpres /= 10;	/*convert into microseconds*/
		tmpres -= DELTA_EPOCH_IN_MICROSECS;
		tv->tv_sec = (long)(tmpres / 1000000UL);
		tv->tv_usec = (long)(tmpres % 1000000UL);
	}

	if (tz != NULL)
	{
		if (!tzflag)
		{
			_tzset();
			tzflag++;
		}

		tz->tz_minuteswest = _timezone / 60;
		tz->tz_dsttime = _daylight;
	}

	return 0;
}

#endif // _MSC_VER

uint TimeGetMilliSecond(void)
{
	struct timeval tv;

	gettimeofday(&tv, NULL);

	return (tv.tv_sec & 0x000FFFFFF) * 1000 + tv.tv_usec / 1000;
}


// ===============================================================================
//
// 											CTooFile
//
// ===============================================================================

CTooFile *CTooFile :: Open(const char *Name, BOOL Warn)
{
	int Mode = O_RDONLY | O_BINARY;
	int Hd = _open(Name, Mode, 0);

	if (Hd < 0)
	{
		if (Warn)
			OS_Printf("Warning: can't find file '%s'\n", Name);
		return NULL;
	}

	CTooFile *Fp = new CTooFile;
	Fp->m_Hd = Hd;
	Fp->m_Pos = 0;
	Fp->m_FileSize = _lseek(Hd, 0, SEEK_END);
	_lseek(Hd, 0, SEEK_SET);
	return Fp;
}


void CTooFile :: Close(void)
{
	_close(m_Hd);
	m_Hd = -1;
	m_FileSize = 0;
	m_Pos = 0;
	delete this;
}


int CTooFile :: Read(void *Dst, long Nb)
{
	return _read(m_Hd, Dst, Nb);
}

int CTooFile :: Seek(int Off, int Mode)
{
	return _lseek(m_Hd, Off, Mode);
}

int CTooFile :: Tell(void)
{
	return Seek(0, SEEK_SET);
}

int CTooFile :: Size(void)
{
	return m_FileSize;
}


CTooFile *CTooFile :: Create(const char *Name, int Mode)
{
	int Hd = _open(Name, Mode | O_TRUNC | O_CREAT, S_IWRITE | S_IREAD);

	if (Hd >= 0)
	{
		CTooFile *Fp = new CTooFile;
		Fp->m_Hd = Hd;
		Fp->m_pMemory = NULL;
		ClearMem(Fp->m_Pos);
		return Fp;
	}

	return NULL;
}


int CTooFile :: Write(const void *Src, int Nb)
{
	int i = _write(m_Hd, Src, Nb);
	m_Pos += i;
	return i;
}


// ============================= FileName manipulation =============================

char *PutExt(char *Name, const char *Ext)
// put the Ext to the Name: PutExt("Toto.Tmp", ".Txt") => "Toto.Txt"
{
	int s = (int) strlen(Name);
	int i = s;

	while (i > 0)
	{
		--i;

		if (Name[i] == '.')
			break;

		if (Name[i] == '\\' || Name[i] == '/' || i == 0)
		{
			i = s;
			break;
		}
	}

	strcpy(Name+i, Ext);
	return Name;
}


char *GetExt(char *Name)
// Get Ext from a filename: GetExt(Toto.Txt) => ".Txt"
{
	int s = (int) strlen(Name);
	int i = s;

	while (i > 0)
	{
		--i;

		if (Name[i] == '.')
			break;

		if (Name[i] == '\\' || Name[i] == '/' || i == 0)
		{
			i = s;
			break;
		}
	}

	return Name+i;
}


// * ======================================================================== *
// * 				                   New / Delete
// * ======================================================================== *

MainFunction g_MyMain = NULL;
int g_NbNew = 0;
BOOL g_InMain = FALSE;

void *NewFunc(uint Size, int Mode)
{
	void *Ptr = malloc(Size);

	if (Ptr == NULL && Size)
	{
		OS_Printf("Error: not enough memory; memory allocation of %d KB failed. "
					"Exit the program & restart it before relaunching the plug-in !\n", Size / 1024);
		throw -1;
	}

	if (g_InMain)
	{
		++g_NbNew;
//		OS_Printf("New (0x%08X) - %d bytes; NbNew = %d\n", Ptr, Size, g_NbNew);
	}

	return Ptr;
}

void DeleteFunc(void *Ptr, int Mode)
{
	if (g_InMain)
	{
		--g_NbNew;
//		OS_Printf("Delete (0x%08X); NbNew = %d\n", Ptr, g_NbNew);
	}

	free(Ptr);
}

//void *operator new(size_t s)
//{
//	return NewFunc((uint) s, 0);
//}
//
//void *operator new[](size_t s)
//{
//	return NewFunc((uint) s, 1);
//}

//void operator delete(void *p)
//{
//	DeleteFunc(p, 0);
//}
//
//void operator delete[](void *p)
//{
//	DeleteFunc(p, 1);
//}


// * ======================================================================== *
// * 				                   CRandom
// * ======================================================================== *

CRandom g_TooRandom;

// use either of the following two sets of parameters
int CRandom::m_q1 = 13, CRandom::m_q2 = 2, CRandom::m_q3 = 3,
		CRandom::m_p1 = 12, CRandom::m_p2 = 4, CRandom::m_p3 = 17;
//int CRandom::m_q1 = 3, CRandom::m_q2 = 2, CRandom::m_q3 = 13,
//	 	CRandom::m_p1 = 20, CRandom::m_p2 = 16, CRandom::m_p3 = 7;

uint CRandom::m_mask1, CRandom::m_mask2, CRandom::m_mask3;
int CRandom::m_shft1, CRandom::m_shft2, CRandom::m_shft3;

float CRandom :: Get(void)
{
	uint b;

	++m_Call;

	b = ((m_s1 << m_q1) ^ m_s1) >> m_shft1;
	m_s1 = ((m_s1 & m_mask1) << m_p1) ^ b;
	b = ((m_s2 << m_q2) ^ m_s2) >> m_shft2;
	m_s2 = ((m_s2 & m_mask2) << m_p2) ^ b;
	b = ((m_s3 << m_q3) ^ m_s3) >> m_shft3;
	m_s3 = ((m_s3 & m_mask3) << m_p3) ^ b;

	return (float) ((m_s1 ^ m_s2 ^ m_s3) / (65536.0 * 65536.0));
}


void CRandom :: SaveSeed(void)
{
	m_bs1 = m_s1;
	m_bs2 = m_s2;
	m_bs3 = m_s3;
}


void CRandom :: RestoreSeed(void)
{
	m_s1 = m_bs1;
	m_s2 = m_bs2;
	m_s3 = m_bs3;
}


void CRandom :: SetSeed(uint a, uint b, uint c)
{
	uint x = 4294967295U;
	int k1 = 31, k2 = 29, k3 = 28;

	m_shft1 = k1 - m_p1;
	m_shft2 = k2 - m_p2;
	m_shft3 = k3 - m_p3;
	m_mask1 = x << (32 - k1);
	m_mask2 = x << (32 - k2);
	m_mask3 = x << (32 - k3);

	while (a <= (uint) (1 << m_shft1))
		a = (a << 1) + 1;

	while (b <= (uint) (1 << m_shft2))
		b = (b << 1) + 1;

	while (c <= (uint) (1 << m_shft3))
		c = (c << 1) + 1;

	ASSERT(a > (uint) (1 << m_shft1));
		m_s1 = a;
	ASSERT(b > (uint) (1 << m_shft2));
		m_s2 = b;
	ASSERT(c > (uint) (1 << m_shft3));
		m_s3 = c;
}


// For measure_risk
#define MAX_SENSITIVE_VAR 99       // Max number of l-diversity sensitive variables (don't go over 9....)

struct SCategory
{
  double value;
  int freq;           //< the total frequency for this category
  int group_freq;       //< the frequency for a specific group/block (used by l-diversity)
  SCategory *pPrev;         // pointer to next category (NULL if first in the list)
  SCategory *pNext;         // pointer to previous category (NULL if last in the list)
};

typedef struct
{
  int position;             //< Stata variable position

  // missing values
  int Nb_Missing_Value;       //< the number of missing values
  int Nb_Missing_Value_In_Group;  //< the number of missing values for current group

  // categories
  int Nb_Category;            //< number of categories
  SCategory *pFirstCategory;        //< pointer to the first category in the list
  SCategory *pLastCategory;       //< pointer to the last category in the list

  // l-diversity (input)
  int Require_Ldiversity;       //< level of l-diversity required or not
  int ldiv_do_entropy;          //< flag to compute l-diversity entropy
  int ldiv_do_recursivity;      //< flag to compute l-diversity recursivity

  // l-divserity output variables
  int Ldiversity_Distinct_Pos;    //< Stata variable position for Distinct output
  int Ldiversity_Entropy_Pos;     //< Stata variable position for Entropy output
  int Ldiversity_Recursive_Pos;   //< Stata variable position for Recursivity output

  // l-diversity (output)
  int Group_Distinct_Ldiversity;  //< Computed distinct l-diversity for current group
  double Group_Entropy_Ldiversity;  //< Computed entropy l-diversity entropy for current group
  int Group_Recursive_Ldiversity; //< Computed recursive l-diversity entropy for current group

} SVariable;

/*=========*/
/* GLOBALS */
/*=========*/

struct SConfig
{
  char is_weighted;         //< indicates if the data is weighted
  int Nb_QuasiId_Var;       //< number of quasi-identifier variables
  int weight_var_pos;       //< position of weight variable (0 if unweighted)
  int group_var_pos;        //< position of group variable
  int risk_var_pos;         //< position of risk variable
  int Nb_Sensitive_Var;     //< Number of sensitive variables
  SVariable Sensitive_Var[99]; //< config for up to 5 l-diversity variables (unweighted only)
  float Ldiversity_Recursivity_Constant;

  int Ldiversity_MultiEntropy_Pos;    //< Stata variable position
  int Ldiversity_MultiRecursive_Pos;  //< Stata variable position

  float Group_MultiEntropy_Ldiversity;  //< Computed multi entropy l-diversity entropy for current group
  int Group_MultiRecursive_Ldiversity;  //< Computed multi recursive l-diversity entropy for current group

  double missing_value;       // vlaue of the missing value
};

SConfig g_Config;


// Function to compare against missing value
BOOL SF_IsMissing(double value) {
  return g_Config.missing_value == value;
}

/*=====================*/
/* CATEGORY MANAGEMENT */
/*=====================*/
/**
 * Finds a variable category by value
 */
SCategory *find_var_cat(SVariable var, double value)
{
  SCategory *pCat;

  pCat = var.pFirstCategory;
  while (pCat != NULL)
  {
    if (pCat->value == value)
      return pCat;
    pCat = pCat->pNext;
  }

  return NULL;
}

/**
 * Adds a value to a variable category
 */
void add_var_cat_value(SVariable *p_var, double value)
{
  SCategory *pCat;

  if (!SF_IsMissing(value))
  {
    pCat = find_var_cat(*p_var, value);
    if (pCat == NULL)
    {
      // create a new category
      pCat = (SCategory *)malloc(sizeof(SCategory));
      pCat->value = value;
      pCat->freq = 0;
      pCat->group_freq = 0;
      pCat->pPrev = p_var->pLastCategory;
      pCat->pNext = NULL;

      // update variable
      if (p_var->pFirstCategory == NULL)
        p_var->pFirstCategory = pCat;
      if (p_var->pLastCategory != NULL)
        p_var->pLastCategory->pNext = pCat;
      p_var->pLastCategory = pCat;
      p_var->Nb_Category++;
    }

    pCat->freq++;
    pCat->group_freq++;
  }
  else
  {
    p_var->Nb_Missing_Value++;
    p_var->Nb_Missing_Value_In_Group++;
  }
}

/**
 * Frees variable memory
 */
void free_var(SVariable *p_var)
{
  SCategory *pCat, *pNext;

  if (p_var->pFirstCategory != NULL)
  {
    pCat = p_var->pFirstCategory;
    do
    {
      pNext = pCat->pNext;
      free(pCat);
      pCat = pNext;
    }
    while (pCat != NULL);
  }
}

/**
 * Initialize a variable
 */
void init_var(SVariable *p_var)
{
  p_var->position = 0;
  p_var->Nb_Category = 0;
  p_var->pFirstCategory = NULL;
  p_var->pLastCategory = NULL;
  p_var->Nb_Missing_Value = 0;
  p_var->Require_Ldiversity = FALSE;
  p_var->ldiv_do_entropy = 0;
  p_var->ldiv_do_recursivity = 0;
}

/**
 * Initialize a variable category
 */
void init_var_cat(SCategory *pCat)
{
  pCat->value = 0.0f;//MISSING;
  pCat->freq = -1;
  pCat->pPrev = NULL;
  pCat->pNext = NULL;
}

/**
 * Resets a variable category group frequencies to zero
 */
void reset_var_cat_group_freq(SVariable *p_var)
{
  SCategory *pCat;

  p_var->Nb_Missing_Value_In_Group = 0;
  pCat = p_var->pFirstCategory;
  while (pCat != NULL)
  {
    pCat->group_freq = 0;
    pCat = pCat->pNext;
  }
}

/**
 * Compares two keys and returns 1 if they are equal
 */
int is_same_key_Risk(double key1[], double key2[], int key_size)
{
  int i;
  int rc = 1;

  for (i = 0; i < key_size; i++)
  {
    if (key1[i] != key2[i] && !(SF_IsMissing(key1[i]) && SF_IsMissing(key2[i])))
    {
       // if(key1[i]!=key2[i]) {
      rc = 0;
      break;
    }
  }

  return rc;
}
int is_same_key_Risk1(double key1[], double key2[], int key_size)
{
  int i;
  int rc = 1;

  for (i = 0; i < key_size; i++)
  {
    //if (key1[i] != key2[i] && !(SF_IsMissing(key1[i]) && SF_IsMissing(key2[i])))
    if (key1[i] != key2[i] || (SF_IsMissing(key1[i]) || SF_IsMissing(key2[i])) )
    {
       // if(key1[i]!=key2[i]) {
      rc = 0;
      break;
    }
  }

  return rc;
}

int is_same_key_Risk2(double key1[], double key2[], int key_size) {
  int i;
  int rc = 1;

  for (i = 0; i < key_size; i++)
  {
    if (key1[i] != key2[i] && (!(SF_IsMissing(key1[i]) || SF_IsMissing(key2[i]))))
    {
      rc = 0;
      break;
    }
  }
  return rc;
}

