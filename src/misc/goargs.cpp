/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goargs.h>
#include <gofileio.h>

/** 
 * @brief Read two words from a line of form <key> <arg>.
 * 
 * Reads a line from the given file. 
 * The line may contain only 2 words, the first is stored in keyword and,
 * if given, checked against checkKeyword. If they don't match, an error is written 
 * to the golib.log file and false is returned.
 * Otherwise, the second word is stored in arg.
 *
 * @param file          C file, open for reading.
 * @param keyword       Holds first word when true is returned.
 * @param arg           Holds second word when true is returned.
 * @param checkKeyword  Word to check first word against.
 * 
 * @return True if successful, false otherwise.
 */
bool goReadASCIIKeywordArg (FILE* file, goString& keyword, goString& arg, const char* checkKeyword)
{
    goString temp;
    goFileIO::readASCIILine (file, temp);
    goList<goString> words;
    temp.getWords (words);
    if (words.getSize() != 2)
    {
        goString msg = "goReadASCIIKeywordArg(): line must contain 2 words while looking for ";
        if (checkKeyword)
            msg += checkKeyword;
        else
            msg += "unknown keyword.";
        goLog::warning(msg);
        return false;
    }
    keyword = words.getFrontElement()->elem;
    arg = words.getFrontElement()->next->elem;
    if (checkKeyword)
    {
        if (keyword != checkKeyword)
        {
            goString msg = "goReadASCIIKeywordArg(): got ";
            msg += keyword;
            msg += ", expected ";
            msg += checkKeyword;
            goLog::warning (msg);
            return false;
        }
    }
    return true;
}
