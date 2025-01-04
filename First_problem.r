#!/usr/bin/env Rscript

standard_input <- file("stdin")
open(standard_input)
p=0
m=0

while(667>666)
{
    print("Provide a coding sequence.")

    #We assume that the characters given are valid -capital letters, in english

    #Read string from standard input
    string <- readLines(standard_input, n = 1)

    #Create a vector, that consists of the individual characters of the string given
    input<-unlist(strsplit(string, split=""))

    #The same vector in reverse (I did not use the variable input - I created input1 - for OCD reasons)
    input1<-unlist(strsplit(string, split=""))
    reverse<-rev(input1)

    #Let's assume a coding region consists of (at least) a start codon, an end codon, and an in between codon (9 nucleotides in total)
    if(length(input)>=9) 
    {
        #The minus 2 is important when reaching the last characters of the vector
        for (i in 1:(length(input) - 2)) 
        {
            #Scan for a start codon (just for fun I added the option to detect mRNA sequences not just DNA)
            #The scanning is done by detecting three nucleotides at a time, thats why I added the minus 2 in the above for statement
            
            if (input[i] == "A" && input[i + 1] == "T" && input[i + 2] == "G" | input[i] == "A" && input[i + 1] == "U" && input[i + 2] == "G") 
            {
                #Save the position where the programme finds a start codon
                p<-i

                #Insted of scanning the remaining sequence three nucleotides at a time
                #a vector is created, in each position of which, a string of three characters is saved
                #So since we know the start codon, all that has to be done is to seperate the remaing sequence into strings of three
                #and check if there is a stop codon in any position
                
                input<-input[p:length(input)]
                input<-paste0(input, collapse="")
                input <- unlist(strsplit(input, split="(?<=.{3})", perl=TRUE))

                for(n in 1:(length(input)))
                {
                    if (input[n] == "TGA" || input[n] == "TAG" || input[n] == "TAA" || input[n] == "UGA"|| input[n] == "UAG" || input[n] == "UAA")
                    {
                        input<-input[1:n]
                        print("It is a coding sequence" , quote=F)
                        print("If we were to show the codons, it would look like this: ", quote=F)
                        print(input)
                        print(" ", quote=F)

                        #Had I not put a break command, a printing chaos would have ensued, that would have printed "It is a coding sequence"
                        # a zillion times
                        
                        break
                    
                    }
                }

            #I am not 100% sure why this break is needed, but the programme would crash without it    
            break
            }
        }
        
        #Same thing as before for the reverse sequence
        for (l in 1:(length(reverse) - 2)) 
        {
            if (reverse[l] == "A" && reverse[l + 1] == "T" && reverse[l + 2] == "G" || reverse[l] == "A" && reverse[l + 1] == "U" && reverse[l + 2] == "G") 
            {
                m<-l
                reverse<-reverse[m:(length(reverse))]
                reverse<-paste0(reverse, collapse="")
                reverse <- unlist(strsplit(reverse, split="(?<=.{3})", perl=TRUE))

                for (k in 1:(length(reverse))) 
                {
                    if (reverse[k] == "TGA" || reverse[k] == "TAG" || reverse[k] == "TAA" || reverse[k] == "UGA"|| reverse[k] == "UAG"|| reverse[k] == "UAA")
                    {
                        reverse<-reverse[1:k]
                        print("The reversed sequence is a coding one.", quote=F)
                        print("If we were to show the codons, it would look like this: ", quote=F)
                        print(reverse)
                        print(" ", quote=F)
                        break
                    }
                }
                    
            break
            }
        }
    
        #There is no start codon
        if(p==0 && m==0)
        {
            print("Could not find coding sequence", quote=F)
        }



    }
    #The minimum requirement for a coding sequence was not met
    else
    {
        print("YOU DID NOT FEED ME ENOUGH NUCLEOTIDES, GOODBYE FOREVER", quote=F)
        quit()
    }
}

















