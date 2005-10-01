#include <goshape.h>
#include <gocurve.h>
#include <gopoint.h>
#include <golist.h>
#include <go4vector.h>
#include <govector.h>
#include <gocomplex.h>
#include <golog.h>
#include <assert.h>

class goShapePrivate
{
    public:
        goShapePrivate();
        ~goShapePrivate();

        goCurvef* curve;
};

goShapePrivate::goShapePrivate ()
    : curve (0)
{
}

goShapePrivate::~goShapePrivate ()
{
}

// =====================================

goShape::goShape (goCurvef* c)
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassName ("goShape");
    myPrivate = new goShapePrivate;
    assert (myPrivate);
    this->setCurve (c);
}

goShape::~goShape ()
{
    if (myPrivate)
    {
        if (myPrivate->curve)
        {
            myPrivate->curve->disconnectObject (this);
        }
        delete myPrivate;
        myPrivate = 0;
    }
}

goCurvef* goShape::getCurve()
{
    return myPrivate->curve;
}

const goCurvef* goShape::getCurve() const
{
    return myPrivate->curve;
}

void goShape::setCurve(goCurvef* c)
{
    if (myPrivate->curve)
    {
        myPrivate->curve->disconnectObject (this);
    }
    myPrivate->curve = c;
    if (myPrivate->curve)
        myPrivate->curve->connectObject (this);
}

static void conj (goVector<goComplexf>& v)
{
    goIndex_t size = (goIndex_t)v.getSize();
    for (goIndex_t i = 0; i < size; ++i)
    {
        v[i].im() *= -1.0f;
    }
}

static goDouble procrustesDistance (const goVector<goComplexf>& v1, const goVector<goComplexf>& v2)
{
    goDouble d = v1.conjInnerProduct(v2).abs();
    return 1 - d*d;
}

static void listToVector (const goList<goPointf>& l, goVector<goComplexf>& v)
{
    if (l.isEmpty())
        return;
    goList<goPointf>::ConstElement* el = l.getFrontElement();
    goIndex_t size = (goIndex_t)l.getSize();
    v.setSize (size);
    for (goIndex_t i = 0; i < size; ++i)
    {
        assert (el);
        v[i].re() = el->elem.x;
        v[i].im() = el->elem.y;
        el = el->next;
    }
}

static goDouble procrustesDistance (goList<goPointf>::Element* e1_, goList<goPointf>::ConstElement* e2_, goIndex_t size)
{
    goVector<goComplexf> v1 (size);
    goVector<goComplexf> v2 (size);
    goList<goPointf>::Element* e1 = e1_;
    goList<goPointf>::ConstElement* e2 = e2_;
    for (goIndex_t i = 0; i < size; ++i)
    {
        assert (e1 && e2);
        v1[i].re() = e1->elem.x;
        v1[i].im() = e1->elem.y;
        v2[i].re() = e2->elem.x;
        v2[i].im() = e2->elem.y;
        e1 = e1->next;
        e2 = e2->next;
    }
    return procrustesDistance (v1,v2);
}

/**
 * @brief  Procrustes alignment.
 *
 * Assumes that this shape and the other are centred and normalized.
 * 
 * @param other  Shape to align to.
 *
 * @return True if successful, false otherwise.
 **/
bool goShape::align (goCurvef& curve, const goCurvef& other)
{
    //= Both curves must have the same number of equally spaces points (do resample() prior to alignment).
    if (other.getPoints().getSize() != curve.getPoints().getSize())
    {
        goLog::warning ("goShape::align(): curves have different numbers of points.");
        return false;
    }
    //= Align the this shape to the other by finding the position at which
    //= the distance between the shapes is minimal.

    goIndex_t size = curve.getPoints().getSize();
    goVector<goComplexf> z1 (size);
    goVector<goComplexf> z2 (size);
    goList<goPointf>::Element*      el      = curve.getPoints().getFrontElement();
    goList<goPointf>::ConstElement* elOther = other.getPoints().getFrontElement();
    assert (el && elOther);
    //= Copy the point coordinates to complex number vectors.
    for (goIndex_t i = 0; i < size; ++i)
    {
        z1[i].re() = el->elem.x;
        z1[i].im() = el->elem.y;
        z2[i].re() = elOther->elem.x;
        z2[i].im() = elOther->elem.y;
        if (el->next && elOther->next) //= Just for sanity -- this is obsolete.
        {
            el      = el->next;
            elOther = elOther->next;
        }
    }
    
    //= Permutate the starting point of this curve and find
    //= the starting point with the smallest Procrustes distance.
    goList<goPointf>* pointList = &curve.getPoints();
    assert (pointList);
    pointList->close();
    el = pointList->getFrontElement();
    elOther = other.getPoints().getFrontElement();
    goList<goPointf>::Element* newFront = el;
    assert (el && elOther);
    assert (pointList->isClosed());
    goVectorf distances (size);
    goFloat minDistance = 999999.0f;
    for (goIndex_t i = 0; i < size; ++i)
    {
        assert (el);
        distances[i] = procrustesDistance (el, elOther, size);
        // printf ("Distance: %f\n", distances[i]);
        if (distances[i] < minDistance)
        {
            minDistance = distances[i];
            newFront = el;
            // printf ("New min distance: %f\n", minDistance);
        }
        el = el->next;
    }
    assert (newFront);
    pointList->open (newFront);

    //= Now we have the correct renumbering. Align this shape w.r.t. the other.
    goVector<goComplexf> v1;
    goVector<goComplexf> v2;
    listToVector (*pointList, v1);
    listToVector (other.getPoints(), v2);
    conj(v1);
    //= conj(z2) * z / (conj(z2) * z2)
    goComplexf alpha = (v1 * v2) / v1.square(); 
    std::cout << "alpha == " << alpha << "\n";
    std::cout << "arg(alpha) == " << alpha.arg() << "\n";
    conj(v1);
    // v1 *= alpha;
    //= Copy the result back to the point list.
    el = pointList->getFrontElement();
    for (goIndex_t i = 0; i < size; ++i)
    {
        assert(el);
        v1[i] *= alpha;
        el->elem.x = v1[i].re();
        el->elem.y = v1[i].im();
        el = el->next;
    }
    
    return true;
}

/**
 * @brief 
 *
 * @note The curve of this shape must be moved to its center of mass before this.
 * 
 * @param weights  
 *
 * @return 
 **/
bool goShape::getWeights (goArray<goFloat>& weights) const
{
    const goCurvef* curve = this->getCurve();
    if (!curve)
        return false;
    if (curve->getPoints().isEmpty())
        return false;

    weights.resize (curve->getPoints().getSize());
    curve->getCurvNorm (weights);
    goIndex_t i;
    //= Normalise curvature values to [0,1]
    goFloat maxCurv = weights[0];
    for (i = 0; i < weights.getSize(); ++i)
    {
        // weights[i] *= -log(sqrt(curve2El->elem.x * curve2El->elem.x + curve2El->elem.y * curve2El->elem.y));
        // weights[i] = -log(weights[i]);
        if (maxCurv < weights[i]) 
        {
            maxCurv = weights[i];
        }
        // printf ("Log(Weight): %f\n", weights[i]);
        // if (el->next)
        //    el = el->next;
    }
    maxCurv = 1.0f / maxCurv;
    for (i = 0; i < weights.getSize(); ++i)
    {
        weights[i] *= 3 * maxCurv;
    }

    goList<goPointf>::ConstElement* el = curve->getPoints().getFrontElement();
    assert (el);
    go4Vectorf meanDelta (0.0,0.0);
    //= Assume all points are translated with (-center of mass).
    while (true)
    {
        meanDelta.x += fabs(el->elem.x);
        meanDelta.y += fabs(el->elem.y);
        if (!el->next)
            break;
        el = el->next;
    }
    goIndex_t pointCount = (goIndex_t)curve->getPoints().getSize();
    meanDelta.x /= (float)pointCount;
    meanDelta.y /= (float)pointCount;
  
    goList<go4Vectorf> grad;
    if (!curve->getGrad (grad))
        return false;
    assert (grad.getSize() == pointCount);

    //= Calculate normalisation factor for |delta - mu_delta|
    goFloat maxMeanDeltaDiff = 0.0f;
    go4Vectorf temp (0.0f, 0.0f, 0.0f, 0.0f);
    el = curve->getPoints().getFrontElement();
    for (i = 0; i < pointCount; ++i)
    {
        //= |delta - mu_delta| 
        temp.x = el->elem.x;
        temp.y = el->elem.y;
        temp -= meanDelta;
        if (maxMeanDeltaDiff < temp.abs())
        {
            maxMeanDeltaDiff = temp.abs();
        }
        if (el->next)
            el = el->next;
    }
    maxMeanDeltaDiff = 1.0f / maxMeanDeltaDiff;

    go4Vectorf delta (0.0f, 0.0f, 0.0f, 0.0f);
    el = curve->getPoints().getFrontElement();
    goList<go4Vectorf>::Element* gradEl = grad.getFrontElement();
    assert (gradEl);
    for (i = 0; i < pointCount; ++i)
    {
        //= |delta - mu_delta| -- currently |delta| - |mu_delta|
        temp.x = el->elem.x;
        temp.y = el->elem.y;
        temp -= meanDelta;
        weights[i] += temp.abs() * maxMeanDeltaDiff;
        // weights[i] += (temp.abs() - meanDelta.abs()) * maxMeanDeltaDiff; // temp.abs();
        //= |grad * (delta/|delta|)|
        delta.x = el->elem.x;
        delta.y = el->elem.y;
        goFloat D = delta.abs();
        if (D != 0.0f)
            delta *= 1.0f / D;
        D = gradEl->elem.abs();
        if (D != 0.0f)
            gradEl->elem *= 1.0f / D;
        weights[i] += fabs(gradEl->elem * delta);
        printf ("Weight: %f\n", weights[i]);
        if (gradEl->next)
            gradEl = gradEl->next;
        if (el->next)
            el = el->next;
    }
    return true;
}

/**
 * @brief Translates all points so the origin is in the center of mass.
 *
 * @return True if successful, false otherwise.
 **/
bool goShape::center (goCurvef& curve)
{
    if (curve.getPoints().isEmpty())
        return false;

    goPointf com;
    if (!curve.getCenterOfMass (com))
        return false;

    com *= -1.0f;
    curve.translate (com);
    return true;
}

/**
 * @brief Normalizes point coordinates so that \f$\sum_limits_i x_i^2 + y_i^2 = 1\f$.
 *
 * @return True if successful, false otherwise.
 **/
bool goShape::normalize (goCurvef& curve)
{
    if (curve.getPoints().isEmpty())
        return false;

    goDouble sum = 0.0;
    goList<goPointf>::Element* el = curve.getPoints().getFrontElement();
    assert(el);
    while (true)
    {
        sum += el->elem.x * el->elem.x + el->elem.y * el->elem.y;
        if (!el->next)
            break;
        el = el->next;
    }
    if (sum <= 0.0)
        return false;
    sum = 1.0f / sqrt(sum);
    el = curve.getPoints().getFrontElement();
    assert(el);
    while (true)
    {
        el->elem.x *= sum;
        el->elem.y *= sum;
        if (!el->next)
            break;
        el = el->next;
    }
    return true;
}

bool goShape::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

void goShape::receiveObjectMessage (const goObjectMessage& msg)
{
    switch (msg.myMessageID)
    {
        case GO_OBJECTMESSAGE_DESTRUCTING:
            {
                if (msg.mySender == myPrivate->curve)
                {
                    myPrivate->curve = 0;
                }
            }
            break;
        default:
            break;
    }
    goObjectBase::receiveObjectMessage (msg);
}

/*
 * @note All shapes and meanRet must be of the same size prior to this call.
 */
static void meanShape (goList<goCurvef*>& shapes, goCurvef& meanRet)
{
    goIndex_t size = static_cast<goIndex_t>(shapes.getSize());
    goFixedArray<goList<goPointf>::Element*> shapeEl (size);
    {
        goList<goCurvef*>::Element* el = shapes.getFrontElement();
        for (goIndex_t i = 0; i < size; ++i, el = el->next)
        {
            assert(el);
            shapeEl[i] = el->elem->getPoints().getFrontElement();
        }
    }
    
    goList<goPointf>::Element* meanEl = meanRet.getPoints().getFrontElement();
    assert(meanEl);
    goIndex_t i = 0;
    while (true)
    {
        meanEl->elem.x = 0.0f;
        meanEl->elem.y = 0.0f;
        for (i = 0; i < size; ++i)
        {
            assert (shapeEl[i]);
            meanEl->elem.x += shapeEl[i]->elem.x;
            meanEl->elem.y += shapeEl[i]->elem.y;
            shapeEl[i] = shapeEl[i]->next;
        }
        if (!meanEl->next)
            break;
        meanEl = meanEl->next;
    }
    goFloat factor = 1.0f / static_cast<float>(size);

    meanEl = meanRet.getPoints().getFrontElement();
    while (true)
    {
        meanEl->elem.x *= factor;
        meanEl->elem.y *= factor;
        if (!meanEl->next)
            break;
        meanEl = meanEl->next;
    }
}

static goDouble curveSquareDifference (const goCurvef& c1, const goCurvef& c2)
{
    goDouble ret = 0.0;
    goList<goPointf>::ConstElement* el1 = c1.getPoints().getFrontElement();
    goList<goPointf>::ConstElement* el2 = c2.getPoints().getFrontElement();
    assert (el1 && el2);
    goDouble temp1;
    goDouble temp2;
    while (true)
    {
        temp1 = el1->elem.x - el2->elem.x;
        temp2 = el1->elem.y - el2->elem.y;
        ret += temp1 * temp1 + temp2 * temp2;
        if (!el1->next || !el2->next)
            break;
        el1 = el1->next;
        el2 = el2->next;
    }
    return ret;
}

bool goShape::procrustesFit (goList<goCurvef*>& shapes, goCurvef& meanShapeRet)
{
    goIndex_t shapeCount = (goIndex_t)shapes.getSize();
    if (shapeCount <= 0)
        return false;
    goList<goCurvef*>::Element* el = shapes.getFrontElement();

    //= Center and normalize all shapes.
    assert(el);
    goIndex_t size = static_cast<goIndex_t>(el->elem->getPoints().getSize());   
    while (true)
    {
        assert (el->elem);
        assert (el->elem->getPoints().getSize() == size);
        if (el->elem->getPoints().getSize() != size)
            return false;
        goShape::center (*el->elem);
        goShape::normalize (*el->elem);
        if (!el->next)
            break;
        el = el->next;
    }

    meanShapeRet = *el->elem;
    goCurvef tempCurve;
    meanShapeRet.resample (100,tempCurve);
    meanShapeRet = tempCurve;
    goShape::center    (meanShapeRet);
    goShape::normalize (meanShapeRet);
    goCurvef oldMeanShape = meanShapeRet;
    goIndex_t i;
    //= Go until convergence or max. 1000 iterations.
    for (i = 0; i < 1000; ++i)
    {
        //= Initialize the mean to the first shape.
        el = shapes.getFrontElement();

        //= Align all shapes with respect to the current mean.
        assert(el);
        while (el)
        {
            assert (el->elem);
            // el->elem->resample (100,tempCurve);
            // *el->elem = tempCurve;
            goShape::align (*el->elem, meanShapeRet);
            // goShape::center (*el->elem);
            goShape::normalize (*el->elem);
            el = el->next;
        }

        //= Update the mean curve.
        meanShape (shapes, meanShapeRet);
        goShape::center (meanShapeRet);
        goShape::normalize (meanShapeRet);
        goDouble d = curveSquareDifference (oldMeanShape, meanShapeRet);
        printf ("d == %f\n", d);
        if (d < 1e-15)
        {
            printf ("goShape::procrustesFit(): %d iterations.\n", i);
            break;
        }
        oldMeanShape = meanShapeRet;
    } 
    return true;
}

#include <golist.hpp>
template class goList<goShape*>;
