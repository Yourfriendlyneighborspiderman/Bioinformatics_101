#include <stdio.h>
#include <math.h>
#include <string.h>

int main()
{

	char seq[ 5000 ];
	int max,  i, len;
	max = 0;

		while ( scanf( "%s", seq ) ==1 )
			
			{
				len = strlen( seq );
				if  ( len > max )
			
					{
						max = len;
					}
			}
	
	printf( "The maximum length is %d\n", max );

}
