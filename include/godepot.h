/* this is --*- C++ -*-- */

#ifndef __GO_DEPOT_H__
#define __GO_DEPOT_H__

#include <iostream.h>
#include <stdlib.h>
#include <gotypes.h>
#include <gopaper.h>
#include <goarray.h>

extern const char* GOP_TICKER;
extern const char* GOP_NAME;
extern const char* GOP_NR;
extern const char* GOP_AMOUNT;
extern const char* GOP_BUYPRICE;

extern const char* GOP_SELLPRICE;

extern const char* GOP_COSTS;

extern const char* GOP_SOLD;

extern const char* GOP_BUYDATE;

extern const char* GOP_SELLDATE;

extern const char* GOP_CURRENCY;

extern const char* GOP_FOREIGN;

extern const char* GOP_TIMEOUT;

extern const char* GOP_INTEREST;

extern const char* GOP_PREPAID;

extern const char* GOP_XCHGBUY;

extern const char* GOP_XCHGSELL;

extern const char* GOP_TYPE;


extern const char* GOP_SECTION_START;

extern const char* GOP_SECTION_END;
//extern const int   GOP_NUMBER_OF_ENTRIES;

/**
 * Class for handling a depot containing goPaper-type papers.
 * To do: add support for goConfigFile input.
 */
class
goDepot {
 public:
  ///
  goDepot();
  ///
  goIndex_t addPaper    (goPaper& p);
  ///
  void      removePaper (unsigned int idx);
  /**
   * Splits a paper and adds a new paper entry so that two paper entries
   * with the old amount (together) exist.
   * returns the new index.
   */
  goIndex_t splitPaper     (goIndex_t idx, goInt32 splitAmount);
  /*
   * splitPaper vice versa.
   */
  goIndex_t mergePapers    (goIndex_t idx1, goIndex_t idx2);
  ///
  void      read 	   (const char* file);
  void      read 	   (goString& file);    //not yet
  ///
  void      write          (const char* file);
  void      write 	   (goString& file);    //not yet

  ///
  goArray <goIndex_t>& findPaperByNr     (const char* nr);
  ///
  goArray <goIndex_t>& findPaperByTicker (const char* ticker);
  ///
  goArray <goIndex_t>& findPaperByName   (const char* name);
  
  ///
  goArray<class goPaper*>& getPapers () { return papers; }
  /**
   * Functions for calculation of gain / mixed prices etc.
   */
  //
  ///
  goFloat gain 		(goArray<goIndex_t>& p);
  ///
  goFloat overallGain 	();
  ///
  goFloat totalValue	();
  //

  ///
  goDepot& operator= (goDepot& other);
 protected:
  ///
  goArray<class goPaper*> papers;
  ///
  goArray<goIndex_t>      searchResults;
};

#endif /* __GO_DEPOT_H__ */

























