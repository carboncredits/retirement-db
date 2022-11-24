// Generated by atdts from type definitions in 'retirement_data.atd'.
//
// Type-safe translations from/to JSON
//
// For each type 'Foo', there is a pair of functions:
// - 'writeFoo': convert a 'Foo' value into a JSON-compatible value.
// - 'readFoo': convert a JSON-compatible value into a TypeScript value
//   of type 'Foo'.


export type Version = {
  major: Int;
  minor: Int;
  patch?: Int;
}

export type FlightTrip =
| { kind: 'None' /* JSON: "none" */ }
| { kind: 'One_way' /* JSON: "one-way" */ }
| { kind: 'Round_trip' /* JSON: "round-trip" */ }

export type FlightClass =
| { kind: 'First' /* JSON: "first" */ }
| { kind: 'Business' /* JSON: "business" */ }
| { kind: 'Premium_economy' /* JSON: "premium-economy" */ }
| { kind: 'Economy' /* JSON: "economy" */ }

export type FlightType =
| { kind: 'Scheduled' /* JSON: "scheduled" */ }
| { kind: 'Chartered' /* JSON: "chartered" */ }

export type Airport = {
  id: string;
  name: string;
  iata_code: string;
}

export type FlightDetails = {
  departure: Airport;
  arrival: Airport;
  passenger_count: Int;
  flight_count: Int;
  travel_class?: string;
  charter?: string;
  aircraft_type: string;
}

export type TrainClass =
| { kind: 'Standard' /* JSON: "standard" */ }
| { kind: 'First_class' /* JSON: "first-class" */ }

export type TrainDetails = {
  origin: string;
  destination: string;
  via: string[];
  train_class: TrainClass;
  train_type: Option<string>;
  number_of_people: Int;
}

export type JourneyTime =
| { kind: 'Zero_to_five' /* JSON: "0-5m" */ }
| { kind: 'Five_to_ten' /* JSON: "5-10m" */ }
| { kind: 'Ten_to_twenty_five' /* JSON: "10-25m" */ }
| { kind: 'Twenty_five_to_fifty' /* JSON: "25-50m" */ }
| { kind: 'Fifty_plus' /* JSON: "50m+" */ }

export type TaxiType =
| { kind: 'Electric' /* JSON: "electric" */ }
| { kind: 'Hybrid' /* JSON: "hybrid" */ }
| { kind: 'Standard' /* JSON: "standard" */ }
| { kind: 'Executive' /* JSON: "executive" */ }
| { kind: 'Airport_shuttle' /* JSON: "airport shuttle" */ }

export type TaxiDetails = {
  journey_time: JourneyTime;
  number_of_people: Int;
  taxi_type: TaxiType;
}

export type AdditionalDetails = {
  journey_time: JourneyTime;
  number_of_people: Int;
  type_: string;
}

export type Reason =
| { kind: 'Conference' }
| { kind: 'Meeting_with_collaborators' /* JSON: "Meeting with collaborators" */ }
| { kind: 'Giving_an_invited_talk' /* JSON: "Giving an invited talk/lecture" */ }
| { kind: 'PhD_viva' /* JSON: "PhD viva" */ }
| { kind: 'Fundraising_activities' /* JSON: "Fundraising activities" */ }
| { kind: 'Recruitment_of_university_staff' /* JSON: "Recruitment of university staff" */ }
| { kind: 'Research' }
| { kind: 'Student_field_trip' /* JSON: "Student field trip" */ }
| { kind: 'Administrative_meetings' /* JSON: "Administrative meetings" */ }
| { kind: 'Other' }

export type TravelDetails = {
  flight_details: FlightDetails[];
  train_details: TrainDetails[];
  taxi_details: TaxiDetails[];
  additional_details: AdditionalDetails[];
  primary_reason: Reason;
  secondary_reason?: Reason;
  reason_text: string;
}

export type GrantDetails = {
  sponsor_and_pi_confirmation: boolean;
  award: string;
  project: string;
  task: string;
}

export type CostCentreDetails = {
  budget_holder_confirmation: boolean;
  department: string;
  cost_centre: string;
  source: string;
}

export type FinanceKind =
| { kind: 'Grant' }
| { kind: 'CostCentre' }

export type CambridgeId = {
  crsid: string;
  name: string;
  department: string;
}

export type Offset = {
  token_id: Int;
  project_name: string;
  minter: string;
  kyc: string;
  amount: Int;
}

export type T = {
  version: Version;
  details: TravelDetails;
  id: CambridgeId;
  finance_kind: FinanceKind;
  offset: Offset;
  cost_centre_details?: CostCentreDetails;
  grant_details?: GrantDetails;
}

export type TList = T[]

export type OnChain = {
  version: Version;
  total_distance: number;
  total_co2e: number;
  number_of_flights: Int;
  hash: string;
}

export type SetRequest = {
  path: string[];
  value: T;
}

export type GetHashRequest = {
  commit: string;
  path: string[];
}

export type GetContentRequest = {
  hash: string;
}

export type StringResponse = {
  errors: string[];
  data: string;
}

export type TResponse = {
  errors: string[];
  data: T;
}

export function writeVersion(x: Version, context: any = x): any {
  return {
    'major': _atd_write_required_field('Version', 'major', _atd_write_int, x.major, x),
    'minor': _atd_write_required_field('Version', 'minor', _atd_write_int, x.minor, x),
    'patch': _atd_write_optional_field(_atd_write_int, x.patch, x),
  };
}

export function readVersion(x: any, context: any = x): Version {
  return {
    major: _atd_read_required_field('Version', 'major', _atd_read_int, x['major'], x),
    minor: _atd_read_required_field('Version', 'minor', _atd_read_int, x['minor'], x),
    patch: _atd_read_optional_field(_atd_read_int, x['patch'], x),
  };
}

export function writeFlightTrip(x: FlightTrip, context: any = x): any {
  switch (x.kind) {
    case 'None':
      return 'none'
    case 'One_way':
      return 'one-way'
    case 'Round_trip':
      return 'round-trip'
  }
}

export function readFlightTrip(x: any, context: any = x): FlightTrip {
  switch (x) {
    case 'none':
      return { kind: 'None' }
    case 'one-way':
      return { kind: 'One_way' }
    case 'round-trip':
      return { kind: 'Round_trip' }
    default:
      _atd_bad_json('FlightTrip', x, context)
      throw new Error('impossible')
  }
}

export function writeFlightClass(x: FlightClass, context: any = x): any {
  switch (x.kind) {
    case 'First':
      return 'first'
    case 'Business':
      return 'business'
    case 'Premium_economy':
      return 'premium-economy'
    case 'Economy':
      return 'economy'
  }
}

export function readFlightClass(x: any, context: any = x): FlightClass {
  switch (x) {
    case 'first':
      return { kind: 'First' }
    case 'business':
      return { kind: 'Business' }
    case 'premium-economy':
      return { kind: 'Premium_economy' }
    case 'economy':
      return { kind: 'Economy' }
    default:
      _atd_bad_json('FlightClass', x, context)
      throw new Error('impossible')
  }
}

export function writeFlightType(x: FlightType, context: any = x): any {
  switch (x.kind) {
    case 'Scheduled':
      return 'scheduled'
    case 'Chartered':
      return 'chartered'
  }
}

export function readFlightType(x: any, context: any = x): FlightType {
  switch (x) {
    case 'scheduled':
      return { kind: 'Scheduled' }
    case 'chartered':
      return { kind: 'Chartered' }
    default:
      _atd_bad_json('FlightType', x, context)
      throw new Error('impossible')
  }
}

export function writeAirport(x: Airport, context: any = x): any {
  return {
    'id': _atd_write_required_field('Airport', 'id', _atd_write_string, x.id, x),
    'name': _atd_write_required_field('Airport', 'name', _atd_write_string, x.name, x),
    'iataCode': _atd_write_required_field('Airport', 'iata_code', _atd_write_string, x.iata_code, x),
  };
}

export function readAirport(x: any, context: any = x): Airport {
  return {
    id: _atd_read_required_field('Airport', 'id', _atd_read_string, x['id'], x),
    name: _atd_read_required_field('Airport', 'name', _atd_read_string, x['name'], x),
    iata_code: _atd_read_required_field('Airport', 'iataCode', _atd_read_string, x['iataCode'], x),
  };
}

export function writeFlightDetails(x: FlightDetails, context: any = x): any {
  return {
    'departure': _atd_write_required_field('FlightDetails', 'departure', writeAirport, x.departure, x),
    'arrival': _atd_write_required_field('FlightDetails', 'arrival', writeAirport, x.arrival, x),
    'passengerCount': _atd_write_required_field('FlightDetails', 'passenger_count', _atd_write_int, x.passenger_count, x),
    'flightCount': _atd_write_required_field('FlightDetails', 'flight_count', _atd_write_int, x.flight_count, x),
    'travelClass': _atd_write_optional_field(_atd_write_string, x.travel_class, x),
    'charter': _atd_write_optional_field(_atd_write_string, x.charter, x),
    'aircraftType': _atd_write_required_field('FlightDetails', 'aircraft_type', _atd_write_string, x.aircraft_type, x),
  };
}

export function readFlightDetails(x: any, context: any = x): FlightDetails {
  return {
    departure: _atd_read_required_field('FlightDetails', 'departure', readAirport, x['departure'], x),
    arrival: _atd_read_required_field('FlightDetails', 'arrival', readAirport, x['arrival'], x),
    passenger_count: _atd_read_required_field('FlightDetails', 'passengerCount', _atd_read_int, x['passengerCount'], x),
    flight_count: _atd_read_required_field('FlightDetails', 'flightCount', _atd_read_int, x['flightCount'], x),
    travel_class: _atd_read_optional_field(_atd_read_string, x['travelClass'], x),
    charter: _atd_read_optional_field(_atd_read_string, x['charter'], x),
    aircraft_type: _atd_read_required_field('FlightDetails', 'aircraftType', _atd_read_string, x['aircraftType'], x),
  };
}

export function writeTrainClass(x: TrainClass, context: any = x): any {
  switch (x.kind) {
    case 'Standard':
      return 'standard'
    case 'First_class':
      return 'first-class'
  }
}

export function readTrainClass(x: any, context: any = x): TrainClass {
  switch (x) {
    case 'standard':
      return { kind: 'Standard' }
    case 'first-class':
      return { kind: 'First_class' }
    default:
      _atd_bad_json('TrainClass', x, context)
      throw new Error('impossible')
  }
}

export function writeTrainDetails(x: TrainDetails, context: any = x): any {
  return {
    'origin': _atd_write_required_field('TrainDetails', 'origin', _atd_write_string, x.origin, x),
    'destination': _atd_write_required_field('TrainDetails', 'destination', _atd_write_string, x.destination, x),
    'via': _atd_write_required_field('TrainDetails', 'via', _atd_write_array(_atd_write_string), x.via, x),
    'trainClass': _atd_write_required_field('TrainDetails', 'train_class', writeTrainClass, x.train_class, x),
    'trainType': _atd_write_required_field('TrainDetails', 'train_type', _atd_write_option(_atd_write_string), x.train_type, x),
    'numberOfPeople': _atd_write_required_field('TrainDetails', 'number_of_people', _atd_write_int, x.number_of_people, x),
  };
}

export function readTrainDetails(x: any, context: any = x): TrainDetails {
  return {
    origin: _atd_read_required_field('TrainDetails', 'origin', _atd_read_string, x['origin'], x),
    destination: _atd_read_required_field('TrainDetails', 'destination', _atd_read_string, x['destination'], x),
    via: _atd_read_required_field('TrainDetails', 'via', _atd_read_array(_atd_read_string), x['via'], x),
    train_class: _atd_read_required_field('TrainDetails', 'trainClass', readTrainClass, x['trainClass'], x),
    train_type: _atd_read_required_field('TrainDetails', 'trainType', _atd_read_option(_atd_read_string), x['trainType'], x),
    number_of_people: _atd_read_required_field('TrainDetails', 'numberOfPeople', _atd_read_int, x['numberOfPeople'], x),
  };
}

export function writeJourneyTime(x: JourneyTime, context: any = x): any {
  switch (x.kind) {
    case 'Zero_to_five':
      return '0-5m'
    case 'Five_to_ten':
      return '5-10m'
    case 'Ten_to_twenty_five':
      return '10-25m'
    case 'Twenty_five_to_fifty':
      return '25-50m'
    case 'Fifty_plus':
      return '50m+'
  }
}

export function readJourneyTime(x: any, context: any = x): JourneyTime {
  switch (x) {
    case '0-5m':
      return { kind: 'Zero_to_five' }
    case '5-10m':
      return { kind: 'Five_to_ten' }
    case '10-25m':
      return { kind: 'Ten_to_twenty_five' }
    case '25-50m':
      return { kind: 'Twenty_five_to_fifty' }
    case '50m+':
      return { kind: 'Fifty_plus' }
    default:
      _atd_bad_json('JourneyTime', x, context)
      throw new Error('impossible')
  }
}

export function writeTaxiType(x: TaxiType, context: any = x): any {
  switch (x.kind) {
    case 'Electric':
      return 'electric'
    case 'Hybrid':
      return 'hybrid'
    case 'Standard':
      return 'standard'
    case 'Executive':
      return 'executive'
    case 'Airport_shuttle':
      return 'airport shuttle'
  }
}

export function readTaxiType(x: any, context: any = x): TaxiType {
  switch (x) {
    case 'electric':
      return { kind: 'Electric' }
    case 'hybrid':
      return { kind: 'Hybrid' }
    case 'standard':
      return { kind: 'Standard' }
    case 'executive':
      return { kind: 'Executive' }
    case 'airport shuttle':
      return { kind: 'Airport_shuttle' }
    default:
      _atd_bad_json('TaxiType', x, context)
      throw new Error('impossible')
  }
}

export function writeTaxiDetails(x: TaxiDetails, context: any = x): any {
  return {
    'journeyTime': _atd_write_required_field('TaxiDetails', 'journey_time', writeJourneyTime, x.journey_time, x),
    'numberOfPeople': _atd_write_required_field('TaxiDetails', 'number_of_people', _atd_write_int, x.number_of_people, x),
    'taxiType': _atd_write_required_field('TaxiDetails', 'taxi_type', writeTaxiType, x.taxi_type, x),
  };
}

export function readTaxiDetails(x: any, context: any = x): TaxiDetails {
  return {
    journey_time: _atd_read_required_field('TaxiDetails', 'journeyTime', readJourneyTime, x['journeyTime'], x),
    number_of_people: _atd_read_required_field('TaxiDetails', 'numberOfPeople', _atd_read_int, x['numberOfPeople'], x),
    taxi_type: _atd_read_required_field('TaxiDetails', 'taxiType', readTaxiType, x['taxiType'], x),
  };
}

export function writeAdditionalDetails(x: AdditionalDetails, context: any = x): any {
  return {
    'journeyTime': _atd_write_required_field('AdditionalDetails', 'journey_time', writeJourneyTime, x.journey_time, x),
    'numberOfPeople': _atd_write_required_field('AdditionalDetails', 'number_of_people', _atd_write_int, x.number_of_people, x),
    'type': _atd_write_required_field('AdditionalDetails', 'type_', _atd_write_string, x.type_, x),
  };
}

export function readAdditionalDetails(x: any, context: any = x): AdditionalDetails {
  return {
    journey_time: _atd_read_required_field('AdditionalDetails', 'journeyTime', readJourneyTime, x['journeyTime'], x),
    number_of_people: _atd_read_required_field('AdditionalDetails', 'numberOfPeople', _atd_read_int, x['numberOfPeople'], x),
    type_: _atd_read_required_field('AdditionalDetails', 'type', _atd_read_string, x['type'], x),
  };
}

export function writeReason(x: Reason, context: any = x): any {
  switch (x.kind) {
    case 'Conference':
      return 'Conference'
    case 'Meeting_with_collaborators':
      return 'Meeting with collaborators'
    case 'Giving_an_invited_talk':
      return 'Giving an invited talk/lecture'
    case 'PhD_viva':
      return 'PhD viva'
    case 'Fundraising_activities':
      return 'Fundraising activities'
    case 'Recruitment_of_university_staff':
      return 'Recruitment of university staff'
    case 'Research':
      return 'Research'
    case 'Student_field_trip':
      return 'Student field trip'
    case 'Administrative_meetings':
      return 'Administrative meetings'
    case 'Other':
      return 'Other'
  }
}

export function readReason(x: any, context: any = x): Reason {
  switch (x) {
    case 'Conference':
      return { kind: 'Conference' }
    case 'Meeting with collaborators':
      return { kind: 'Meeting_with_collaborators' }
    case 'Giving an invited talk/lecture':
      return { kind: 'Giving_an_invited_talk' }
    case 'PhD viva':
      return { kind: 'PhD_viva' }
    case 'Fundraising activities':
      return { kind: 'Fundraising_activities' }
    case 'Recruitment of university staff':
      return { kind: 'Recruitment_of_university_staff' }
    case 'Research':
      return { kind: 'Research' }
    case 'Student field trip':
      return { kind: 'Student_field_trip' }
    case 'Administrative meetings':
      return { kind: 'Administrative_meetings' }
    case 'Other':
      return { kind: 'Other' }
    default:
      _atd_bad_json('Reason', x, context)
      throw new Error('impossible')
  }
}

export function writeTravelDetails(x: TravelDetails, context: any = x): any {
  return {
    'flightDetails': _atd_write_required_field('TravelDetails', 'flight_details', _atd_write_array(writeFlightDetails), x.flight_details, x),
    'trainDetails': _atd_write_required_field('TravelDetails', 'train_details', _atd_write_array(writeTrainDetails), x.train_details, x),
    'taxiDetails': _atd_write_required_field('TravelDetails', 'taxi_details', _atd_write_array(writeTaxiDetails), x.taxi_details, x),
    'additionalDetails': _atd_write_required_field('TravelDetails', 'additional_details', _atd_write_array(writeAdditionalDetails), x.additional_details, x),
    'primaryReason': _atd_write_required_field('TravelDetails', 'primary_reason', writeReason, x.primary_reason, x),
    'secondaryReason': _atd_write_optional_field(writeReason, x.secondary_reason, x),
    'reasonText': _atd_write_required_field('TravelDetails', 'reason_text', _atd_write_string, x.reason_text, x),
  };
}

export function readTravelDetails(x: any, context: any = x): TravelDetails {
  return {
    flight_details: _atd_read_required_field('TravelDetails', 'flightDetails', _atd_read_array(readFlightDetails), x['flightDetails'], x),
    train_details: _atd_read_required_field('TravelDetails', 'trainDetails', _atd_read_array(readTrainDetails), x['trainDetails'], x),
    taxi_details: _atd_read_required_field('TravelDetails', 'taxiDetails', _atd_read_array(readTaxiDetails), x['taxiDetails'], x),
    additional_details: _atd_read_required_field('TravelDetails', 'additionalDetails', _atd_read_array(readAdditionalDetails), x['additionalDetails'], x),
    primary_reason: _atd_read_required_field('TravelDetails', 'primaryReason', readReason, x['primaryReason'], x),
    secondary_reason: _atd_read_optional_field(readReason, x['secondaryReason'], x),
    reason_text: _atd_read_required_field('TravelDetails', 'reasonText', _atd_read_string, x['reasonText'], x),
  };
}

export function writeGrantDetails(x: GrantDetails, context: any = x): any {
  return {
    'sponsorAndPiConfirmation': _atd_write_required_field('GrantDetails', 'sponsor_and_pi_confirmation', _atd_write_bool, x.sponsor_and_pi_confirmation, x),
    'award': _atd_write_required_field('GrantDetails', 'award', _atd_write_string, x.award, x),
    'project': _atd_write_required_field('GrantDetails', 'project', _atd_write_string, x.project, x),
    'task': _atd_write_required_field('GrantDetails', 'task', _atd_write_string, x.task, x),
  };
}

export function readGrantDetails(x: any, context: any = x): GrantDetails {
  return {
    sponsor_and_pi_confirmation: _atd_read_required_field('GrantDetails', 'sponsorAndPiConfirmation', _atd_read_bool, x['sponsorAndPiConfirmation'], x),
    award: _atd_read_required_field('GrantDetails', 'award', _atd_read_string, x['award'], x),
    project: _atd_read_required_field('GrantDetails', 'project', _atd_read_string, x['project'], x),
    task: _atd_read_required_field('GrantDetails', 'task', _atd_read_string, x['task'], x),
  };
}

export function writeCostCentreDetails(x: CostCentreDetails, context: any = x): any {
  return {
    'budgetHolderConfirmation': _atd_write_required_field('CostCentreDetails', 'budget_holder_confirmation', _atd_write_bool, x.budget_holder_confirmation, x),
    'department': _atd_write_required_field('CostCentreDetails', 'department', _atd_write_string, x.department, x),
    'costCentre': _atd_write_required_field('CostCentreDetails', 'cost_centre', _atd_write_string, x.cost_centre, x),
    'source': _atd_write_required_field('CostCentreDetails', 'source', _atd_write_string, x.source, x),
  };
}

export function readCostCentreDetails(x: any, context: any = x): CostCentreDetails {
  return {
    budget_holder_confirmation: _atd_read_required_field('CostCentreDetails', 'budgetHolderConfirmation', _atd_read_bool, x['budgetHolderConfirmation'], x),
    department: _atd_read_required_field('CostCentreDetails', 'department', _atd_read_string, x['department'], x),
    cost_centre: _atd_read_required_field('CostCentreDetails', 'costCentre', _atd_read_string, x['costCentre'], x),
    source: _atd_read_required_field('CostCentreDetails', 'source', _atd_read_string, x['source'], x),
  };
}

export function writeFinanceKind(x: FinanceKind, context: any = x): any {
  switch (x.kind) {
    case 'Grant':
      return 'Grant'
    case 'CostCentre':
      return 'CostCentre'
  }
}

export function readFinanceKind(x: any, context: any = x): FinanceKind {
  switch (x) {
    case 'Grant':
      return { kind: 'Grant' }
    case 'CostCentre':
      return { kind: 'CostCentre' }
    default:
      _atd_bad_json('FinanceKind', x, context)
      throw new Error('impossible')
  }
}

export function writeCambridgeId(x: CambridgeId, context: any = x): any {
  return {
    'crsid': _atd_write_required_field('CambridgeId', 'crsid', _atd_write_string, x.crsid, x),
    'name': _atd_write_required_field('CambridgeId', 'name', _atd_write_string, x.name, x),
    'department': _atd_write_required_field('CambridgeId', 'department', _atd_write_string, x.department, x),
  };
}

export function readCambridgeId(x: any, context: any = x): CambridgeId {
  return {
    crsid: _atd_read_required_field('CambridgeId', 'crsid', _atd_read_string, x['crsid'], x),
    name: _atd_read_required_field('CambridgeId', 'name', _atd_read_string, x['name'], x),
    department: _atd_read_required_field('CambridgeId', 'department', _atd_read_string, x['department'], x),
  };
}

export function writeOffset(x: Offset, context: any = x): any {
  return {
    'tokenId': _atd_write_required_field('Offset', 'token_id', _atd_write_int, x.token_id, x),
    'projectName': _atd_write_required_field('Offset', 'project_name', _atd_write_string, x.project_name, x),
    'minter': _atd_write_required_field('Offset', 'minter', _atd_write_string, x.minter, x),
    'kyc': _atd_write_required_field('Offset', 'kyc', _atd_write_string, x.kyc, x),
    'amount': _atd_write_required_field('Offset', 'amount', _atd_write_int, x.amount, x),
  };
}

export function readOffset(x: any, context: any = x): Offset {
  return {
    token_id: _atd_read_required_field('Offset', 'tokenId', _atd_read_int, x['tokenId'], x),
    project_name: _atd_read_required_field('Offset', 'projectName', _atd_read_string, x['projectName'], x),
    minter: _atd_read_required_field('Offset', 'minter', _atd_read_string, x['minter'], x),
    kyc: _atd_read_required_field('Offset', 'kyc', _atd_read_string, x['kyc'], x),
    amount: _atd_read_required_field('Offset', 'amount', _atd_read_int, x['amount'], x),
  };
}

export function writeT(x: T, context: any = x): any {
  return {
    'version': _atd_write_required_field('T', 'version', writeVersion, x.version, x),
    'details': _atd_write_required_field('T', 'details', writeTravelDetails, x.details, x),
    'id': _atd_write_required_field('T', 'id', writeCambridgeId, x.id, x),
    'financeKind': _atd_write_required_field('T', 'finance_kind', writeFinanceKind, x.finance_kind, x),
    'offset': _atd_write_required_field('T', 'offset', writeOffset, x.offset, x),
    'costCentreDetails': _atd_write_optional_field(writeCostCentreDetails, x.cost_centre_details, x),
    'grantDetails': _atd_write_optional_field(writeGrantDetails, x.grant_details, x),
  };
}

export function readT(x: any, context: any = x): T {
  return {
    version: _atd_read_required_field('T', 'version', readVersion, x['version'], x),
    details: _atd_read_required_field('T', 'details', readTravelDetails, x['details'], x),
    id: _atd_read_required_field('T', 'id', readCambridgeId, x['id'], x),
    finance_kind: _atd_read_required_field('T', 'financeKind', readFinanceKind, x['financeKind'], x),
    offset: _atd_read_required_field('T', 'offset', readOffset, x['offset'], x),
    cost_centre_details: _atd_read_optional_field(readCostCentreDetails, x['costCentreDetails'], x),
    grant_details: _atd_read_optional_field(readGrantDetails, x['grantDetails'], x),
  };
}

export function writeTList(x: TList, context: any = x): any {
  return _atd_write_array(writeT)(x, context);
}

export function readTList(x: any, context: any = x): TList {
  return _atd_read_array(readT)(x, context);
}

export function writeOnChain(x: OnChain, context: any = x): any {
  return {
    'version': _atd_write_required_field('OnChain', 'version', writeVersion, x.version, x),
    'totalDistance': _atd_write_required_field('OnChain', 'total_distance', _atd_write_float, x.total_distance, x),
    'totalCo2e': _atd_write_required_field('OnChain', 'total_co2e', _atd_write_float, x.total_co2e, x),
    'numberOfFlights': _atd_write_required_field('OnChain', 'number_of_flights', _atd_write_int, x.number_of_flights, x),
    'hash': _atd_write_required_field('OnChain', 'hash', _atd_write_string, x.hash, x),
  };
}

export function readOnChain(x: any, context: any = x): OnChain {
  return {
    version: _atd_read_required_field('OnChain', 'version', readVersion, x['version'], x),
    total_distance: _atd_read_required_field('OnChain', 'totalDistance', _atd_read_float, x['totalDistance'], x),
    total_co2e: _atd_read_required_field('OnChain', 'totalCo2e', _atd_read_float, x['totalCo2e'], x),
    number_of_flights: _atd_read_required_field('OnChain', 'numberOfFlights', _atd_read_int, x['numberOfFlights'], x),
    hash: _atd_read_required_field('OnChain', 'hash', _atd_read_string, x['hash'], x),
  };
}

export function writeSetRequest(x: SetRequest, context: any = x): any {
  return {
    'path': _atd_write_required_field('SetRequest', 'path', _atd_write_array(_atd_write_string), x.path, x),
    'value': _atd_write_required_field('SetRequest', 'value', writeT, x.value, x),
  };
}

export function readSetRequest(x: any, context: any = x): SetRequest {
  return {
    path: _atd_read_required_field('SetRequest', 'path', _atd_read_array(_atd_read_string), x['path'], x),
    value: _atd_read_required_field('SetRequest', 'value', readT, x['value'], x),
  };
}

export function writeGetHashRequest(x: GetHashRequest, context: any = x): any {
  return {
    'commit': _atd_write_required_field('GetHashRequest', 'commit', _atd_write_string, x.commit, x),
    'path': _atd_write_required_field('GetHashRequest', 'path', _atd_write_array(_atd_write_string), x.path, x),
  };
}

export function readGetHashRequest(x: any, context: any = x): GetHashRequest {
  return {
    commit: _atd_read_required_field('GetHashRequest', 'commit', _atd_read_string, x['commit'], x),
    path: _atd_read_required_field('GetHashRequest', 'path', _atd_read_array(_atd_read_string), x['path'], x),
  };
}

export function writeGetContentRequest(x: GetContentRequest, context: any = x): any {
  return {
    'hash': _atd_write_required_field('GetContentRequest', 'hash', _atd_write_string, x.hash, x),
  };
}

export function readGetContentRequest(x: any, context: any = x): GetContentRequest {
  return {
    hash: _atd_read_required_field('GetContentRequest', 'hash', _atd_read_string, x['hash'], x),
  };
}

export function writeStringResponse(x: StringResponse, context: any = x): any {
  return {
    'errors': _atd_write_required_field('StringResponse', 'errors', _atd_write_array(_atd_write_string), x.errors, x),
    'data': _atd_write_required_field('StringResponse', 'data', _atd_write_string, x.data, x),
  };
}

export function readStringResponse(x: any, context: any = x): StringResponse {
  return {
    errors: _atd_read_required_field('StringResponse', 'errors', _atd_read_array(_atd_read_string), x['errors'], x),
    data: _atd_read_required_field('StringResponse', 'data', _atd_read_string, x['data'], x),
  };
}

export function writeTResponse(x: TResponse, context: any = x): any {
  return {
    'errors': _atd_write_required_field('TResponse', 'errors', _atd_write_array(_atd_write_string), x.errors, x),
    'data': _atd_write_required_field('TResponse', 'data', writeT, x.data, x),
  };
}

export function readTResponse(x: any, context: any = x): TResponse {
  return {
    errors: _atd_read_required_field('TResponse', 'errors', _atd_read_array(_atd_read_string), x['errors'], x),
    data: _atd_read_required_field('TResponse', 'data', readT, x['data'], x),
  };
}


/////////////////////////////////////////////////////////////////////
// Runtime library
/////////////////////////////////////////////////////////////////////

export type Int = number

export type Option<T> = null | { value: T }

function _atd_missing_json_field(type_name: string, json_field_name: string) {
    throw new Error(`missing field '${json_field_name}'` +
                    ` in JSON object of type '${type_name}'`)
}

function _atd_missing_ts_field(type_name: string, ts_field_name: string) {
    throw new Error(`missing field '${ts_field_name}'` +
                    ` in TypeScript object of type '${type_name}'`)
}

function _atd_bad_json(expected_type: string, json_value: any, context: any) {
  let value_str = JSON.stringify(json_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible JSON value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_bad_ts(expected_type: string, ts_value: any, context: any) {
  let value_str = JSON.stringify(ts_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible TypeScript value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_check_json_tuple(len: Int, x: any, context: any) {
  if (! Array.isArray(x) || x.length !== len)
    _atd_bad_json('tuple of length ' + len, x, context);
}

function _atd_read_unit(x: any, context: any): null {
  if (x === null)
    return null
  else {
    _atd_bad_json('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_json('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_int(x: any, context: any): Int {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_json('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_json('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_json('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_required_field<T>(type_name: string,
                                     field_name: string,
                                     read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined) {
    _atd_missing_json_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return read_elt(x, context)
}

function _atd_read_optional_field<T>(read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined || x === null)
    return x
  else
    return read_elt(x, context)
}

function _atd_read_field_with_default<T>(read_elt: (x: any, context: any) => T,
                                         default_: T,
                                         x: any,
                                         context: any): T {
  if (x === undefined || x === null)
    return default_
  else
    return read_elt(x, context)
}

function _atd_read_option<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => Option<T> {
  function read_option(x: any, context: any): Option<T> {
    if (x === 'None')
      return null
    else {
      _atd_check_json_tuple(2, x, context);
      switch (x[0]) {
        case 'Some':
          return { value: read_elt(x[1], context) }
        default:
          _atd_bad_json('option', x, context)
          throw new Error('impossible')
      }
    }
  }
  return read_option
}

function _atd_read_nullable<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => T | null {
  function read_nullable(x: any, context: any): T | null {
    if (x === null)
      return null
    else
      return read_elt(x, context)
  }
  return read_nullable
}

function _atd_read_array<T>(read_elt: (x: any, context: any) => T):
  (elts: any, context: any) => T[] {
  function read_array(elts: any, context: any): T[] {
    if (Array.isArray(elts))
      return elts.map((x) => read_elt(x, elts))
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_array
}

function _atd_read_assoc_array_into_map<K, V>(
    read_key: (key: any, context: any) => K,
    read_value: (value: any, context: any) => V
  ): (x: any, context: any) => Map<K, V> {
  function read_assoc(elts: any, context: any): Map<K, V> {
    if (Array.isArray(elts)) {
      const res = new Map<K, V>([])
      for (const x of elts) {
        if (Array.isArray(x) && x.length === 2)
          res.set(read_key(x[0], x), read_value(x[1], x))
        else {
          _atd_bad_json('pair', x, elts)
          throw new Error('impossible')
        }
      }
      return res
    }
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_map<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => Map<string, T> {
  function read_assoc(elts: any, context: any): Map<string, T> {
    if (typeof elts === 'object') {
      const res = new Map<string, T>([])
      for (const [key, value] of Object.entries(elts))
        res.set(key, read_value(value, elts))
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_array<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => [string, T][] {
  function read_assoc(elts: any, context: any): [string, T][] {
    if (typeof elts === 'object') {
      const res: [string, T][] = []
      for (const [key, value] of Object.entries(elts))
        res.push([key, read_value(value, elts)])
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_write_unit(x: any, context: any) {
  if (x === null)
    return x
  else {
    _atd_bad_ts('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_ts('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_int(x: any, context: any): Int {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_ts('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_ts('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_ts('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_option<T>(write_elt: (x: T, context: any) => any):
   (elts: Option<T>, context: any) => any {
  function write_option(x: Option<T>, context: any): any {
    if (x === null)
      return 'None'
    else
      return ['Some', write_elt(x.value, context)]
  }
  return write_option
}

function _atd_write_nullable<T>(write_elt: (x: T, context: any) => any):
  (x: T | null, context: any) => any {
  function write_option(x: T | null, context: any): any {
    if (x === null)
      return null
    else
      return write_elt(x, context)
  }
  return write_option
}

function _atd_write_array<T>(write_elt: (elt: T, context: any) => any):
  (elts: T[], context: any) => any {
  return ((elts: T[], context: any): any =>
    elts.map((x) => write_elt(x, elts))
  )
}

function _atd_write_assoc_map_to_array<K, V>(
    write_key: (key: K, context: any) => any,
    write_value: (value: V, context: any) => any
  ): (elts: Map<K, V>, context: any) => any {
  function write_assoc(elts: Map<K, V>, context: any): any {
    const res: any = []
    elts.forEach((value: V, key: K) =>
      res.push([write_key(key, elts), write_value(value, elts)])
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_map_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: Map<string, T>, context: any) => any {
  function write_assoc(elts: Map<string, T>, context: any): any {
    const res: any = {}
    elts.forEach((value: T, key: string) =>
      res[key] = write_value(value, elts)
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_array_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: [string, T][], context: any) => any {
  function write_assoc(elts: [string, T][], context: any): any {
    const res: any = {}
    for (const [key, value] of elts)
      res[key] = write_value(value, elts)
    return res
  }
  return write_assoc
}

function _atd_write_required_field<T>(type_name: string,
                                      field_name: string,
                                      write_elt: (x: T, context: any) => any,
                                      x: T,
                                      context: any): any {
  if (x === undefined) {
    _atd_missing_ts_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return write_elt(x, context)
}

function _atd_write_optional_field<T>(write_elt: (x: T, context: any) => any,
                                      x: T,
                                      context: any): any {
  if (x === undefined || x === null)
    return x
  else
    return write_elt(x, context)
}

function _atd_write_field_with_default<T>(
  write_elt: (x: T, context: any) => any,
  default_: T,
  x: T,
  context: any
): T {
  const value = (x === undefined || x === null) ? default_ : x
  return write_elt(value, context)
}

