#include <gocache.h>
#include <goerror.h>

goCache::goCache()
{
    maxSize = 0;
    currentSize = 0;
    freeIndices.resize(0);
}

goCache::~goCache()
{
    usage.resize(0);
    id.resize (0);
    freeIndices.resize (0);
}

void
goCache::cacheSetMaxSize(goSize_t max)
{
    maxSize = max;
}

bool
goCache::cacheAddElement (goSize_t identifier, goSize_t sz)
{
    while ( (currentSize + sz) > maxSize )
	{
	    /*
	     * Das aufrufende Programm muss Elemente loeschen bis genug Speicher vorhanden ist.
	     */
	    return false;
	}
    /*
     * Neues Element dazu
     */ 
    goSize_t newIndex;
    if (freeIndices.getSize() > 0)
	{
	    newIndex = freeIndices[freeIndices.getSize()-1];
	    freeIndices.resize(freeIndices.getSize()-1);
	    goSize_t idx;
	    // Alle Eintraege zusammenruecken und neuen Eintrag am Ende anfuegen
	    for (idx = newIndex + 1; idx < (goSize_t)usage.getSize(); idx++)
		{
		    usage[idx - 1] = usage[idx];
		}
	    idx--;
	    usage[idx] = 1;
	    id[idx] = identifier;
	    size[idx] = sz;
	} else
	    {
		usage += 1;
		id += identifier;
		size += sz;
		newIndex = usage.getSize() - 1;
	    }
    currentSize += sz;
    return true;
}

goSize_t
goCache::cacheRemoveElement ()
{
    /*
     * Das letzte Element in den arrays sollte das bisher am wenigsten benutzte sein.
     * Das einfach mal rausschmeissen.
     */
    goSize_t retval = id[id.getSize() - 1];
    id.resize (id.getSize() - 1);
    usage.resize (usage.getSize() - 1);
    currentSize -= size[size.getSize() - 1];
    size.resize (size.getSize() - 1);
    return retval;
}

void
goCache::cacheRemoveElement (goSize_t identifier)
{
    /*
     * Das letzte Element in den arrays sollte das bisher am wenigsten benutzte sein.
     * Das einfach mal rausschmeissen.
     */

    goIndex_t i;
    for (i = 0; i < id.getSize(); i++)
	{
	    if (id[i] == identifier)
		{
		    // Tag id as free:
		    freeIndices += i;
		    currentSize -= size[i];
		    size[i] = 0;
		    break;
		    // id.remove(i);
		    // usage.remove(i);
		    // size.remove(i);
		}
	}
}

/*
 * Too slow?
 */
void
goCache::cacheAccessElement (goSize_t identifier)
{
    goSize_t i;
    for (i = 0; i < usage.getSize(); i++)
	{
	    if (id[i] == identifier)
		{
		    usage[i]++;
		    // Nach lings verschieben bis nur groessere links vom Element stehen
		    while ( (i > 0) && (usage[i] > usage[i - 1]) )
			{
			    goSize_t tmp;
			    tmp = usage[i - 1];
			    usage[i - 1] = usage[i];
			    usage[i] = tmp;

			    tmp = id[i - 1];
			    id[i - 1] = id[i];
			    id[i] = tmp;
			    i--;
			}
		    break;
		}
	}
    if (i == usage.getSize())
	{
	    goError::print("goCache::cacheAccessElement()","Accessed element NOT FOUND IN INTERNAL STRUCTURES!");
	}
}


