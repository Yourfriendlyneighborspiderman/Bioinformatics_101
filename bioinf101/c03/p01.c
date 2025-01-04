#include <stdio.h>
#include <math.h>
#include <string.h>

int main()
{

	char seq[ 5000 ];
	int  i, len;
	float gc;
	gc = 0;

	scanf( "%s", seq );
	len = strlen( seq );

		for ( i=0 ; i<len ; i++)
		
			{
				if ( seq[i] == 'G' || seq[i] == 'C' )
				
					{
						gc++;
					}


			}

	gc=(gc/len)*100;
	printf( "The GC percentage of the sequence is %f \n", gc );

}
