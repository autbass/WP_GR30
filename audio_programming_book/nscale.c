/*
Listing 1.5.2: The Audio Programming Book.
BUGS: Segmentation fault(core dumped) if called without arguments. 
-> SOLVED. if (arg != 3) obvsly has to be the first statement to catch wrong arg numbers.
Display E.T frequencies for n N-note octave, from a given MIDI note
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char *argv[])
{
    /* Declare variables here */
    int notes, midinote, i;
    double frequency, ratio;
    double intervals[25]; /* changed to 25 to be able to display octave as well.*/
    double c0, c5;

    /*  Section used to catch wrong arguments given in argument vector. */

    if (argc != 3)
    {
        printf("Program nscale.\n");
        printf("enter 2 numerical values, 1st defines in how many equally space intervals an octave should be divided.\n");
        printf("E.G. 12 for equal temperament, 2nd in which midi range notes should be given.");
        printf("accepted values 1-24, 1-127\n");
        return 1;
    }
    notes = atoi(argv[1]); /* read argument 1 given in function call, convert to integer from string */
    if (notes < 1)
    {
        printf("Error: notes must be positive\n");
        return 1;
    }
    if (notes > 24)
    {
        printf("Error: maximum value for notes is 24\n");
        return 1;
    }
    midinote = atoi(argv[2]); /* read argument 2 given in function call, convert to integer from string */
    if (midinote < 0)
    {
        printf("Error: cannot have negative MIDI notes!\n");
        return 1;
    }

    if (midinote > 127)
    {
        printf("Error: maximum MIDInote is 127!\n");
        return 1;
    }

    /* find the frequency of the MIDI note. */

    /* Calculate standard E.T semitone ratio*/
    ratio = pow(2.0, 1.0 / 12.0);
    /*find Middle C, three semitones above low A=220*/
    c5 = 220.0 * pow(ratio, 3);
    /*MIDI note 0 is C, 5 octaves below Middle C*/
    c0 = c5 * pow(0.5, 5);
    frequency = c0 * pow(ratio, midinote);
    /* calc ratio from notes, and fill the frequency array*/
    ratio = pow(2.0, 1.0 / notes);

    for (i = 0; i <= notes; i++)
    {
        intervals[i] = frequency;
        frequency *= ratio;
    }

    for (i = 0; i <= notes; i++)
    {
        printf("%i: %f Hz\n", i, intervals[i]);
    }

    return 0;
}