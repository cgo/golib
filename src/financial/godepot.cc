#include <ctype.h>
#include <iostream.h>
#include <fstream.h>
#include <godepot.h>
#include <gostring.h>
#include <string.h>

/* For adding config file support some time ... */
#include <goconfigfileaccess.h>

#ifndef isblank
#define isblank(c)      __isctype((c), _ISblank)
#endif

goDepot::
goDepot() {
}

goIndex_t
goDepot::
addPaper (goPaper& p) {
  papers.resize(papers.size() + 1);
  papers[papers.size()-1] = new goPaper (p);
  return (goIndex_t) (papers.size()-1);
}

void 
goDepot::
removePaper (unsigned int idx) {
  goArray<class goPaper*> tmp_papers;
  goIndex_t i = 0;
  
  for (i = 0; i < papers.getSize(); i++) {
    if ((unsigned)i != idx) {
      tmp_papers.resize(tmp_papers.getSize()+1);
      tmp_papers[tmp_papers.getSize()-1] = papers[i];
    }
  }
  delete papers[idx];
  papers = tmp_papers;
}

goIndex_t
goDepot::
splitPaper (goIndex_t idx,
	    goInt32   splitAmount) {
  goPaper tmpPaper;
  goFloat tmpCosts;

  if (idx > papers.getSize()) {
    return 0;
  }
  if (splitAmount > (goInt32)papers[idx]->getAmount()) {
    return 0;
  }
  tmpPaper = *papers[idx];
  tmpCosts = (goFloat) ( (papers[idx]->getAmount() - splitAmount) * papers[idx]->getCosts() / (float) (papers[idx]->getAmount()) );
  papers[idx]->setAmount (papers[idx]->getAmount() - splitAmount);
  tmpPaper.setCosts  (papers[idx]->getCosts() - tmpCosts);
  papers[idx]->setCosts (tmpCosts);
  tmpPaper.setAmount (splitAmount);
  return addPaper (tmpPaper);
}

goIndex_t
goDepot::
mergePapers (goIndex_t idx1,
	     goIndex_t idx2) {
  goPaper tmpPaper;
  goFloat mixPrice = 0;
  goFloat price1  = papers[idx1]->getBuyPrice();
  goFloat price2  = papers[idx2]->getBuyPrice();
  goInt32 amount1 = papers[idx1]->getAmount();
  goInt32 amount2 = papers[idx2]->getAmount();

  mixPrice = (goFloat) ( ((price1 * amount1) + (price2 * amount2)) / (float) (amount1 + amount2) );

  tmpPaper = *papers[idx1];
  tmpPaper.setAmount   (amount1 + amount2);
  tmpPaper.setCosts    (papers[idx1]->getCosts() + papers[idx2]->getCosts());
  tmpPaper.setBuyPrice (mixPrice);

  if (idx1 < idx2) {
    removePaper (idx2);
    removePaper (idx1);
  } else {
    if (idx1 > idx2) {
    removePaper (idx1);
    removePaper (idx2);
    }
  }

  return addPaper (tmpPaper);
}

/*
void
goDepot::new_read (const char* file) {
  goConfigFileAccess f;
  f.read (file);
  
  goIndex_t chapterCount, sectionCount;
  goArray<goConfigFileChapter*>* chapters = &f.getChapterArray();
  goArray<goConfigFileSection*>* sections;
  if (chapters) {
    for (chapterCount = 0; chapterCount < chapters->getSize(); chapterCount++) {
      
    }
  }

}
*/

void
goDepot::
read (const char* file) {
  enum STATES {
    FIRST_CHAR,
    READ_ID,
    READ_VALUE,
    READ_COMMENT,
    READ_SECTION,
    SECTION_READ,
    SECTION_END,
    FILE_END
  };
  goString str,str2;			// strings to read stuff in
  unsigned char EQ = '=';		// equal 
  unsigned char CM = '#';		// comment lines
  unsigned char SECTION_DELIM1 = '[';	// section delimiters
  unsigned char SECTION_DELIM2 = ']';	// section delimiters

  bool     read_error = false;		// error flag
  unsigned char c = 0;			// temporary char
  STATES   state;			// state machine states
  ifstream f;				// file stream
  goPaper  p;				// the paper to read stuff in
  int      numOfEntries;		/* OBSOLETE but still used*/
					
  bool entry_found = false;	

  f.open (file);
  if (f.fail()) {
    cout << "error opening file " << file << "\n";
    return;
  }
  str = "";
  state = FIRST_CHAR;
  numOfEntries = 0;

  while (!f.eof()) {
    switch (state) {
    case FIRST_CHAR:
      f.get (c);
      if ( (c == EQ) || (isblank(c)) || (c == '\n')) { 
	state = READ_COMMENT;
      } else {
	if (c == CM) {
	  state = READ_COMMENT;
	} else {
	  if (c == SECTION_DELIM1) {
	    state = READ_SECTION;
	  } else {
	    state = READ_ID;
	  }	
	} 
      }
      break;
    case READ_SECTION:
      f.get (c);	
      while ( (c != SECTION_DELIM2) && (c != '\n') && (f.eof() == 0) ) {
	if (!isblank (c)) {
	  str += c;
	}
	f.get(c);
      }
      if (f.eof()) { state = FILE_END; }
      else { state = SECTION_READ; }
      break;
    case SECTION_READ:
      if (str == GOP_SECTION_START) {
	str = "";
	state = FIRST_CHAR;
      } else { 
	if (str == GOP_SECTION_END) {
	        /*
		 * If we have all entries, add the paper to the depot and reset 
		 * the counter.
		 */
	  this->addPaper (p);
	  numOfEntries = 0;	
	}
      }
      str = "";
      state = FIRST_CHAR;
      break;
    case READ_COMMENT:
      while ( (c != '\n') && !f.eof()) {
	f.get (c);
      }
      if (f.eof()) { state = FILE_END;}
      else state = FIRST_CHAR;
      break;
    case READ_ID:
      while ( (c != EQ) && (c != '\n') && (f.eof() == 0) ) {
	if (!isblank (c)) {
	  str += c;	
	}
	f.get(c);
      }
      state = FILE_END;
      if ( c == '\n' ) {
	f.get(c);
	state = READ_ID;
      }
      if ( f.eof() ) {
	cout << "unexpected end of file in goDepot\n";
	state = FILE_END;
      }
      if ( c == EQ ) {
	state = READ_VALUE;
      }
      break;
    case READ_VALUE:
      entry_found = false;
      str2 = "";
      while ( (isblank(c) || (c == EQ)) && !f.eof() ) {
	f.get(c);
      }
      while ( (c != '\n') && (f.eof() == 0) ) {
	str2 += c;	
	f.get(c);
      }

      if (str == GOP_TICKER) {
	p.setTicker (str2);
	entry_found = true;
      }
      if (str == GOP_NAME) {
	p.setName (str2);
	entry_found = true;
      }
      if (str == GOP_NR) {
	p.setNr (str2);
	entry_found = true;
      }
      if (str == GOP_AMOUNT) {
	p.setAmount ((goInt32)str2.toInt());
	entry_found = true;
      }
      if (str == GOP_BUYPRICE) {
	p.setBuyPrice ((goFloat)str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_SELLPRICE) {
	p.setSellPrice ((goFloat)str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_COSTS) {
	p.setCosts ((goFloat)str2.toFloat());		
	entry_found = true;
      }
      if (str == GOP_SOLD) {
	p.setSold (str2.toBool());
	entry_found = true;
      }
      if (str == GOP_BUYDATE) {
	p.setBuyDate (str2.toDate());
	entry_found = true;
      }
      if (str == GOP_SELLDATE) {
	p.setSellDate (str2.toDate ());
	entry_found = true;
      }
      if (str == GOP_CURRENCY) {
	p.setCurrency (str2);
	entry_found = true;
      }
      if (str == GOP_FOREIGN) {
	p.setForeignCurrency (str2);
	entry_found = true;
      }
      if (str == GOP_TIMEOUT) {
	p.setTimeOut (str2.toDate());
	entry_found = true;
      }
      if (str == GOP_INTEREST) {
	p.setInterest (str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_PREPAID) {
	p.setPrePaid (str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_XCHGBUY) {
	p.setExchangeRateBuy (str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_XCHGSELL) {
	p.setExchangeRateSell (str2.toFloat());
	entry_found = true;
      }
      if (str == GOP_TYPE) {
	if (str2 == "loan") {
	  p.setType (goPaper::LOAN);
	} else {
	  if (str2 == "share") {
	    p.setType (goPaper::SHARE);
	  } else {
	    p.setType (goPaper::UNKNOWN);
	  }
	}	
	entry_found = true;
      }
      str = "";
      if (entry_found) {
	numOfEntries++;	
      }
      state = FIRST_CHAR;
      break;
    case FILE_END:
      if (read_error) {
	cout << "read error occured.\n";
      }
      read_error = false;
      f.close();
      return;
    default: break;
    }
  }
}

void
goDepot::write (const char* file) {
  goIndex_t i = 0;
  ofstream f;
  f.open (file);
  if (f.fail()) {
    return;
  }
  for (i = 0; i < papers.getSize(); i++) {
    f << "\n# Index number " << i << "\n";
    f << "[" << GOP_SECTION_START << "]\n";
    f << *papers[i];
    f << "[" << GOP_SECTION_END << "]\n";
  }
  f.close();
}

/*
 * Searches for number nr and puts all indices matching it into searchResults.
 */
goArray<goIndex_t>&
goDepot::
findPaperByNr (const char* nr) {
  goIndex_t i = 0;
  searchResults.resize(0);
  for (i = 0; i < papers.getSize(); i++) {
    if (papers[i]->getNr() == nr) {
      searchResults+=i;
    } 
  }
  return searchResults;
}

/*
 * Searches for ticker ticker and puts all indices matching it into searchResults.
 */
goArray<goIndex_t>&
goDepot::
findPaperByTicker (const char* ticker) {
  goIndex_t i = 0;
  searchResults.resize(0);
  for (i = 0; i < papers.getSize(); i++) {
    if (papers[i]->getTicker() == ticker) {
      searchResults+=i;
    } 
  }
  return searchResults;
}

/*
 * Searches for name n and puts all indices matching it into searchResults.
 */
goArray<goIndex_t>&
goDepot::
findPaperByName (const char* n) {
  goIndex_t i = 0;
  searchResults.resize(0);
  for (i = 0; i < papers.getSize(); i++) {
    if (papers[i]->getName() == n) {
      searchResults+=i;
    } 
  }
  return searchResults;
}

/*
 * Calculates the actual gain (or loss) of the papers with indices in p.
 * The gain value is "absolute", it represents the difference 
 * sum(SELLPRICE) - sum(BUYPRICE) - sum(COSTS).
 * At this moment, no interests or anything is taken into account.
 */
goFloat
goDepot::
gain (goArray<goIndex_t>& p) {
  goFloat totalgain = 0.0;
  goIndex_t i = 0;

  for (i = 0; i < p.getSize(); i++) {
    totalgain += papers[p[i]]->getGain();
  }
  return (goFloat)(totalgain);
}

goFloat
goDepot::
overallGain () {
  goArray<goIndex_t> p;
  goIndex_t i = 0;
  p.resize (papers.getSize());
  for (i = 0; i < papers.getSize(); i++) {
    p[i]= i;
  }
  return gain (p);
}

goFloat
goDepot::
totalValue () {
  goFloat value = 0;
  goIndex_t   i = 0;
  for (i = 0; i < papers.getSize(); i++) {
    if (!papers[i]->isSold()) {
      value += papers[i]->getValue();
    }
  }
  return value;
}

goDepot& 
goDepot::operator= (goDepot& other) {
  goIndex_t i = 0;
  papers.resize (other.getPapers().getSize());
  for (i = 0; i < papers.getSize(); i++) {
    papers[i] = new goPaper();
    *papers[i] = *other.getPapers()[i];
  }
  return (*this);
}




