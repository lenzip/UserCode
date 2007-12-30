#ifndef SiTrackerMRHTools_SimpleDAFHitCollector_h
#define SiTrackerMRHTools_SimpleDAFHitCollector_h
#include "RecoTracker/SiTrackerMRHTools/interface/MultiRecHitCollector.h"
#include <vector>

class Propagator;
class MeasurementEstimator;
class SiTrackerMultiRecHitUpdator;

class SimpleDAFHitCollector :public MultiRecHitCollector {
	public:
	explicit SimpleDAFHitCollector(const MeasurementTracker* measurementTracker,
				 const SiTrackerMultiRecHitUpdator* updator,
			         const MeasurementEstimator* est,
				 const Propagator* propagator
				 ):MultiRecHitCollector(measurementTracker), theUpdator(updator), theEstimator(est), thePropagator(propagator){}
			

	virtual ~SimpleDAFHitCollector(){}
	
	//given a trajectory it returns a collection
        //of TSiTrackerMultiRecHits and InvalidTransientRecHits.
        //For each measurement in the trajectory, measurements are looked for according to the 
        //MeasurementDet::fastMeasurements method only in the detector where the original measurement lays. 
        //If measurements are found a TSiTrackerMultiRecHit is built.
	//All the components will lay on the same detector  
	
	virtual std::vector<TrajectoryMeasurement> recHits(const Trajectory&) const;

	const SiTrackerMultiRecHitUpdator* getUpdator() const {return theUpdator;}
	const MeasurementEstimator* getEstimator() const {return theEstimator;}
        const Propagator* getPropagator() const {return thePropagator;}

	private:
	//TransientTrackingRecHit::ConstRecHitContainer buildMultiRecHits(const std::vector<TrajectoryMeasurementGroup>& measgroup) const;
	void buildMultiRecHits(const std::vector<TrajectoryMeasurement>& measgroup, std::vector<TrajectoryMeasurement>& result) const;
	
	private:
	const SiTrackerMultiRecHitUpdator* theUpdator;
	const MeasurementEstimator* theEstimator;
	//this actually is not used in the fastMeasurement method 	
	const Propagator* thePropagator; 
	

};


#endif 
