#include <stdio.h>
#include <math.h>
#include <string.h>

int main()
{

	char seq1[ 5000 ];
	char seq2[ 5000 ];
	int  len1;
	int  len2;

	scanf( "%s", seq1 );
	len1 = strlen( seq1 );
	scanf( "%s", seq2 );
	len2 = strlen( seq2 );
	printf( "The length of the first  sequence is %5d\n", len1 );
	printf( "The length of the second sequence is %5d\n", len2 );

}
