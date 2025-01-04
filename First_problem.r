#!/usr/bin/env Rscript

standard_input <- file("stdin")
open(standard_input)
p=0
m=0

while(4>3)
{
    print("Provide a coding sequence.")
    string <- readLines(standard_input, n = 1)
    input<-unlist(strsplit(string, split=""))
    input1<-unlist(strsplit(string, split=""))

    reverse<-rev(input1)

    if(length(input)>=9)
    {
        for (i in 1:(length(input) - 2)) 
        {
            if (input[i] == "A" && input[i + 1] == "T" && input[i + 2] == "G" | input[i] == "A" && input[i + 1] == "U" && input[i + 2] == "G") 
            {
                p<-i
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
                        break
                    
                    }
                }
            break
            }
        }

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
    
    
        if(p==0 && m==0)
        {
            print("Could not find coding sequence", quote=F)
        }



    }
    else
    {
        print("YOU DID NOT FEED ME ENOUGH NUCLEOTIDES, GOODBYE FOREVER", quote=F)
        quit()
    }
}

















