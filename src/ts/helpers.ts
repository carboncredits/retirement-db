import { TrainClass, JourneyTime, TaxiType, Reason, FlightClass, Version } from "./retirement_data"

// The current version for the on-chain data, this needs bumped
// accordingly when the ATD definition changes
export const on_chain_version : Version = { major: 0, minor: 1 }

export const allFlightClasses: FlightClass[] = [
  { kind: 'First' /* JSON: "first" */ },
  { kind: 'Business' /* JSON: "business" */ },
  { kind: 'Premium_economy' /* JSON: "premium-economy" */ },
  { kind: 'Economy' /* JSON: "economy" */ }
]

export const allTrainClasses: TrainClass[] = [
  { kind: "Standard" },
  { kind: "First_class" },
]

export const allJourneyTime : JourneyTime[] = [
  { kind: 'Zero_to_five' /* JSON: "0-5m" */ },
  { kind: 'Five_to_ten' /* JSON: "5-10m" */ },
  { kind: 'Ten_to_twenty_five' /* JSON: "10-25m" */ },
  { kind: 'Twenty_five_to_fifty' /* JSON: "25-50m" */ },
  { kind: 'Fifty_plus' /* JSON: "50m+" */ }
]

export const allTaxiTypes: TaxiType[] = [
 { kind: 'Electric' /* JSON: "electric" */ },
 { kind: 'Hybrid' /* JSON: "hybrid" */ },
 { kind: 'Standard' /* JSON: "standard" */ },
 { kind: 'Executive' /* JSON: "executive" */ },
 { kind: 'Airport_shuttle' /* JSON: "airport shuttle" */ }
]

export const allReasons: Reason[] = [
 { kind: 'Conference' },
 { kind: 'Meeting_with_collaborators' /* JSON: "Meeting with collaborators" */ },
 { kind: 'Giving_an_invited_talk' /* JSON: "Giving an invited talk/lecture" */ },
 { kind: 'PhD_viva' /* JSON: "PhD viva" */ },
 { kind: 'Fundraising_activities' /* JSON: "Fundraising activities" */ },
 { kind: 'Recruitment_of_university_staff' /* JSON: "Recruitment of university staff" */ },
 { kind: 'Research' },
 { kind: 'Student_field_trip' /* JSON: "Student field trip" */ },
 { kind: 'Administrative_meetings' /* JSON: "Administrative meetings" */ },
 { kind: 'Other' }
]