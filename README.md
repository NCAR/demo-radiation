# demo-radiation
Toy model for Aerosol&ndash;Radiation communication

To build and run the demo:

```
mkdir build
cd build
cmake ..
make
./demo modal
./demo sectional
```
## Requirements

- permit schemes to use whatever wavelength grid they want as is currently done (radiation, photolysis, etc.)

- perform expensive setup for optical property calculations during initialization (reading files, setting up interpolations, etc.)

- remove assumptions of the way aerosols are represented from the radiation code (mode number, size, shape, sectional schemes, etc.)

- have schemes interested in optical properties maintain their own wavelength grids (not have aerosols maintain grids for all schemes)

## Proposal

The relevant parts of this toy model for CCPP are in `model.F90` (the host model) and `radiation.F90` (a radiation scheme). The proposal is essentially to allow an instance of the derived type `aerosol_t` to be registered as a CCPP state variable by an aerosol scheme and have radiation be able to request this variable through CCPP for its initialization and run functions.

(The registration of an `aerosol_t` object by a separate aerosol scheme was left out of this demo for simplicity, and simply created in `model.F90`.)

The structure of this simple `aerosol_t` type is what we expect could be an outcome of the larger aerosol API effort currently being discussed.
