

// Programming languages I course NTUA 2021-2022 
//Given a caesar ciphered string of unknown key (from a txt file) we decipher it using an entropy function in linear time.
// Fardelas Ioannis  & Dimitrios Papanikolopoulos
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include<cstring>
#include <climits>
#include <math.h>
using namespace std;

int main(int argc, char * argv[]) {
 double  freq[26]={0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
0.00978, 0.02360, 0.00150, 0.01974, 0.00074 },alphabet[26];
    int i=0 ,offset=0,k ,j, N ;  
    string input, cipheredstring ;
    
    float sum=0, min=INT_MAX;
    int right;
    for(j=0; j<=25; j++){alphabet[j]=0;}//1)initializing array that will contain frequency of each character found in the string
   
   
     // 2)reading from txt file in the same directory as my executable
    if (argc > 1) {
        //cout << "argv[1] = " << argv[1] << endl; 
    } else {
        cout << "No file name entered. Exiting...";
        return -1;
    }
    ifstream infile(argv[1]);
    if (infile.is_open() && infile.good()) {
        //cout << "File is now open!\nContains:\n";
        string textstring = "";
        while (getline(infile,textstring)){
            input +=textstring+"\n" ;
        }    
    } else {
        cout << "Failed to open file..";
    } 
    //3)generating alphabet array that contains frequency of each character in the given string
    k=input.length();
    for(i=0; i<k; i++){
        if (isalpha(input[i])){
             offset= 65 ;
           if (islower(input[i])) { 
             offset= 97 ;
            }    
       alphabet[(int)input[i]-offset]++ ;
        }
    
    } 
    //cout << input<<endl;
    for(j=0; j<=25; j++){alphabet[j]=alphabet[j]/input.length();}
    
    //4)calculating shift N that minimizes entropy function
    for (int c = 0; c <= 25; c++)
    {
        sum -= alphabet[(c + N) % 26] * log2(freq[c]);
    }
    right=1;
    for (N = 1; N <= 25; N++)
    {
        sum = 0;
        for (int c = 0; c <= 25; c++)
        {
            sum -= alphabet[(c + N) % 26] * log2(freq[c]);
        }
        if (sum < min)
        {
            min = sum;
            right = N;
        }
        //cout << N << "   " << sum << "  " << min << endl;
    }
    //5)shifting encrypted string by the right amount so we get deciphered text
 for(i=0; i< k; i++){
        if (isalpha(input[i])){
             offset= 65 ;
           if (islower(input[i])) { 
             offset= 97 ;
            } 
        int cipher =(((int)input[i]-offset+26-right)%26)+offset;
        cipher = (char)cipher ;
        input[i]=cipher;
        }
    }

    cout << input << endl;
  
    return 0;
}