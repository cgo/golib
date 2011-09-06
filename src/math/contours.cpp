/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <golist.h>
#include <gosignal3d.h>
#include <gosignal3dgenericiterator.h>
#include <gofixedarray.h>
#include <gomath.h>
#include <gocontours.h>

//=
//= Funktioniert noch nicht so gut wie matlab contour() (nur in relativ glatten Faellen).
//= TODO: Irgendwann noch machen: Vom aktuellen Punkt (in calculate()) alle
//= angrenzenden moeglichen Verbindungspunkte in eine queue, oder Baum daraus aufbauen,
//= und mit back tracking zurueck gehen, so dass die laengste Verbindung herauskommt.

namespace goMath
{
    class ContoursLine;

    class ContoursLine
    {
        public:
            enum LineType
            {
                INACTIVE = 0,
                RIGHT = 1,
                DOWN = 2,
                UPRIGHT = 4
            };

            ContoursLine (goSize_t x_ = 0, goSize_t y_ = 0, enum LineType type_ = INACTIVE, goDouble t_ = 0.0) 
                : x(x_), y(y_), type(type_), t(t_), xf(0.0), yf(0.0)
            {
                xf = goDouble(x);
                yf = goDouble(y);
                switch (type)
                {
                    case RIGHT: xf += t; break;
                    case DOWN:  yf += t; break;
                    case UPRIGHT: xf += t; yf += t; break;
                    default: break;
                }
            };
            ~ContoursLine () {};

            goSize_t x, y;
            enum LineType type;
            goDouble t;
            goDouble xf, yf;
    };

    template <class T>
        static goDouble calculate_t2 (const goSignal3DBase<void>& image, goMath::ContoursLine& line, goDouble level, goSize_t& x2, goSize_t& y2)
        {
            goDouble v0 = 0.0, v1 = 1.0;
            switch (line.type)
            {
                case ContoursLine::INACTIVE: return 0.0; break;
                case ContoursLine::RIGHT: 
                                             {
                                                 x2 = line.x + 1;
                                                 y2 = line.y;
                                             }
                                             break;
                case ContoursLine::DOWN: 
                                             {
                                                 x2 = line.x;
                                                 y2 = line.y + 1;
                                             }
                                             break;
                case ContoursLine::UPRIGHT: 
                                             {
                                                 x2 = line.x + 1;
                                                 y2 = line.y - 1;
                                             }
                                             break;
                default: goLog::warning ("goMath::Contours: calculate_t(): illegal value"); return 0.0; break;
            }
            v0 = goDouble(*(const T*)image.getPtr(line.x,line.y,0)) - level;
            v1 = goDouble(*(const T*)image.getPtr(x2, y2, 0)) - level;
            return v0 / (v0 - v1);
        }

    class ContoursMesh
    {
        public:
            ContoursMesh (const goSignal3DBase<void>* image_)
                : visitedLines(), lines(), width(0), height(0), image(image_)
            {
                if (image)
                {
                    this->width = image->getSizeX();
                    this->height = image->getSizeY();
                    this->visitedLines.setSize (width * height);
                    this->lines.setSize (width * height);
                    this->visitedLines.fill (0);
                    this->lines.fill (0);
                }
            };
            ~ContoursMesh ()
            {
            };

            bool visited (goSize_t x, goSize_t y, enum goMath::ContoursLine::LineType lineType)
            {
                return (visitedLines[y * width + x] & lineType);
            };

            enum ContoursLine::LineType getTypes (goSize_t x, goSize_t y)
            {
                if (x >= width || y >= height)
                    return ContoursLine::INACTIVE;
                return (static_cast<enum ContoursLine::LineType>(lines[y * width + x]));
            };

            void setType (goSize_t x, goSize_t y, enum ContoursLine::LineType t)
            {
                lines[y * width + x] |= t;
            };

            void setVisited (const ContoursLine& line, bool set = true)
            {
                this->setVisited (line.x, line.y, line.type, set);
            };

            void setVisited (goSize_t x, goSize_t y, enum goMath::ContoursLine::LineType lineType, bool set = true)
            {
                if (set)
                    visitedLines[y * width + x] |= lineType;
                else
                    visitedLines[y * width + x] &= ~lineType;
            };

            goDouble calculate_t (ContoursLine& line, goDouble level, goSize_t& x2, goSize_t& y2)
            {
                switch (this->image->getDataType().getID())
                {
                    case GO_INT8: return calculate_t2<goInt8> (*image, line, level, x2, y2); break;
                    case GO_UINT8: return calculate_t2<goUInt8> (*image, line, level, x2, y2); break;
                    case GO_INT16: return calculate_t2<goInt16> (*image, line, level, x2, y2); break;
                    case GO_UINT16: return calculate_t2<goUInt16> (*image, line, level, x2, y2); break;
                    case GO_INT32: return calculate_t2<goInt32> (*image, line, level, x2, y2); break;
                    case GO_UINT32: return calculate_t2<goUInt32> (*image, line, level, x2, y2); break;
                    case GO_FLOAT: return calculate_t2<goFloat> (*image, line, level, x2, y2); break;
                    case GO_DOUBLE: return calculate_t2<goDouble> (*image, line, level, x2, y2); break;
                    default: goLog::warning ("goMath::ContoursMesh::calculate_t(): unknown image type."); return 0.0; break;
                }
            };

            bool getNextLine (ContoursLine& current, ContoursLine& nextRet)
            {
                switch (current.type)
                {
                    case ContoursLine::RIGHT:
                        {
                            if (this->getTypes(current.x + 1, current.y - 1) & ContoursLine::DOWN &&
                                !this->visited(current.x + 1, current.y - 1, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x + 1;
                                nextRet.y = current.y - 1;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y - 1) & ContoursLine::DOWN &&
                                !this->visited(current.x, current.y - 1, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y - 1;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x + 1, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x + 1, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x + 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y + 1) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y + 1, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y - 1) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y - 1, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y - 1;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                        }
                        break;
                    case ContoursLine::DOWN:
                        {
                            if (this->getTypes(current.x, current.y) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y + 1) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y + 1, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x - 1, current.y) & ContoursLine::RIGHT &&
                                !this->visited(current.x - 1, current.y, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x - 1, current.y + 1) & ContoursLine::RIGHT &&
                                !this->visited(current.x - 1, current.y + 1, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x + 1, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x + 1, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x + 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x - 1, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x - 1, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                        }
                        break;
#if 0
                    case ContoursLine::RIGHT:
                        {
                            if (this->getTypes(current.x + 1, current.y - 1) & ContoursLine::DOWN &&
                                !this->visited(current.x + 1, current.y - 1, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x + 1;
                                nextRet.y = current.y - 1;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y) & ContoursLine::UPRIGHT &&
                                !this->visited(current.x, current.y, ContoursLine::UPRIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::UPRIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y + 1) & ContoursLine::UPRIGHT &&
                                !this->visited(current.x, current.y + 1, ContoursLine::UPRIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::UPRIGHT;
                                return true;
                            }
                        }
                        break;
                    case ContoursLine::DOWN:
                        {
                            if (this->getTypes(current.x - 1, current.y + 1) & ContoursLine::UPRIGHT &&
                                !this->visited(current.x - 1, current.y + 1, ContoursLine::UPRIGHT))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::UPRIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x - 1, current.y + 1) & ContoursLine::RIGHT &&
                                !this->visited(current.x - 1, current.y + 1, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y + 1) & ContoursLine::UPRIGHT &&
                                !this->visited(current.x, current.y + 1, ContoursLine::UPRIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y + 1;
                                nextRet.type = ContoursLine::UPRIGHT;
                                return true;
                            }
                        }
                        break;
                    case ContoursLine::UPRIGHT:
                        {
                            if (this->getTypes(current.x - 1, current.y) & ContoursLine::DOWN &&
                                !this->visited(current.x - 1, current.y, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x + 1, current.y - 1) & ContoursLine::DOWN &&
                                !this->visited(current.x + 1, current.y - 1, ContoursLine::DOWN))
                            {
                                nextRet.x = current.x + 1;
                                nextRet.y = current.y - 1;
                                nextRet.type = ContoursLine::DOWN;
                                return true;
                            }
                            if (this->getTypes(current.x, current.y) & ContoursLine::RIGHT &&
                                !this->visited(current.x, current.y, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                            if (this->getTypes(current.x - 1, current.y) & ContoursLine::RIGHT &&
                                !this->visited(current.x - 1, current.y, ContoursLine::RIGHT))
                            {
                                nextRet.x = current.x - 1;
                                nextRet.y = current.y;
                                nextRet.type = ContoursLine::RIGHT;
                                return true;
                            }
                        }
                        break;
#endif
                    default: return false; break;
                }
                return false;
            };

            goFixedArray<goUInt8> visitedLines;
            goFixedArray<goUInt8> lines;
            goSize_t width;
            goSize_t height;
            const goSignal3DBase<void>* image;
    };

    class ContoursPrivate
    {
        public:
            ContoursPrivate ()
                : contours ()
            {
            };

            ~ContoursPrivate ()
            {
            };

            goList<goMath::Matrixd> contours;
    };

};

goMath::Contours::Contours ()
{
    myPrivate = new goMath::ContoursPrivate;
}

goMath::Contours::~Contours ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class T>
static void checkZeros (const goSignal3DBase<void>& image,
        goList<goMath::ContoursLine>& lines,
        goDouble level)
{
    goSignal3DGenericConstIterator it (&image);
    goSize_t sx = image.getSizeX();
    goSize_t sy = image.getSizeY();

    for (goSize_t y = 0; y < sy - 1; ++y)
    {
        it.resetX();
        for (goSize_t x = 0; x < sx - 1; ++x)
        {   
            goDouble v0 = goDouble(*(T*)*it) - level;
            goDouble v1 = goDouble(*(T*)it.rightX()) - level;
            goDouble v2 = goDouble(*(T*)it.rightY()) - level;
            goDouble v3 = goDouble(*(T*)it.rightDown()) - level;
            if (v0 * v1 <= 0.0)
            {
                goDouble t = v0 / (v0 - v1);
                lines.append (goMath::ContoursLine (x, y, goMath::ContoursLine::RIGHT, t));
            }
            if (v0 * v2 <= 0.0)
            {
                goDouble t = v0 / (v0 - v2);
                lines.append (goMath::ContoursLine (x, y, goMath::ContoursLine::DOWN, t));
            }
//            if (v2 * v3 < 0.0)
//            {
//                goDouble t = v2 / (v2 - v1);
//                lines.append (goMath::ContoursLine (x, y+1, goMath::ContoursLine::UPRIGHT, t));
//            }

            it.incrementX();
        }
        it.incrementY();
    }
}


void goMath::Contours::calculate (const goSignal3DBase<void>& image, goDouble level)
{
    goList<goMath::ContoursLine> lines;

    //= Populate the line list above with lines containing zero crossings:
    switch (image.getDataType().getID())
    {
        case GO_UINT8:  checkZeros<goUInt8>  (image, lines, level); break;
        case GO_INT8:   checkZeros<goInt8>   (image, lines, level); break;
        case GO_UINT16: checkZeros<goUInt16> (image, lines, level); break;
        case GO_INT16:  checkZeros<goInt16>  (image, lines, level); break;
        case GO_UINT32: checkZeros<goUInt32> (image, lines, level); break;
        case GO_INT32:  checkZeros<goInt32>  (image, lines, level); break;
        case GO_FLOAT:  checkZeros<goFloat>  (image, lines, level); break;
        case GO_DOUBLE: checkZeros<goDouble> (image, lines, level); break;
        default: goLog::warning ("goMath::Contours::calculate(): unknown data type."); return; break;
    }

    ContoursMesh mesh (&image);
    //= Set lines having zero crossings:
    {
        goList<goMath::ContoursLine>::Element* el = lines.getFrontElement();
        while (el)
        {
            mesh.setType (el->elem.x, el->elem.y, el->elem.type);
            el = el->next;
        }
    }

    //= Start with any line and follow it.
    // goSize_t x = 0, y = 0, tri = 0;
    goList<ContoursLine>::Element* el = lines.getFrontElement();

    goList<goDouble> path_x;
    goList<goDouble> path_y;

    myPrivate->contours.erase();

    while (el)
    {
        if (!mesh.visited(el->elem.x, el->elem.y, el->elem.type))
        {
            path_x.append (el->elem.xf);
            path_y.append (el->elem.yf);
            mesh.setVisited (el->elem);
            ContoursLine nextLine;
            ContoursLine currentLine = el->elem;
            while (mesh.getNextLine (currentLine, nextLine))
            {
                currentLine = nextLine;
                goDouble temp_x = 0.0, temp_y = 0.0;
                goSize_t x2 = 0, y2 = 0;
                goDouble t = mesh.calculate_t (currentLine, level, x2, y2);
                temp_x = goDouble(currentLine.x) * (1.0 - t) + goDouble(x2) * t;
                temp_y = goDouble(currentLine.y) * (1.0 - t) + goDouble(y2) * t;
                path_x.append (temp_x);
                path_y.append (temp_y);
                mesh.setVisited (currentLine);
            }
            {
                goList<goDouble>::Element* elX = path_x.getFrontElement();
                goList<goDouble>::Element* elY = path_y.getFrontElement();
                myPrivate->contours.append (goMath::Matrixd(0,0));
                goMath::Matrixd& M = myPrivate->contours.getTail();
                goSize_t i = 0;
                goSize_t sz = path_x.getSize();
                M.resize (sz, 2);
                while (elX && i < sz)
                {
                    M(i,0) = elX->elem;
                    M(i,1) = elY->elem;
                    elX = elX->next;
                    elY = elY->next;
                    ++i;
                }
                path_x.erase();
                path_y.erase();
            }
        }
        el = lines.remove (el);
    }
}

goList<goMath::Matrixd>& goMath::Contours::getContours ()
{
    return myPrivate->contours;
}
