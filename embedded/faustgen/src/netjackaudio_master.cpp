/************************************************************************
    FAUST Architecture File
    Copyright (C) 2010-2013 GRAME, Centre National de Creation Musicale
    ---------------------------------------------------------------------
    This Architecture section is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3 of
    the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; If not, see <http://www.gnu.org/licenses/>.

    EXCEPTION : As a special exception, you may create a larger work
    that contains this FAUST architecture section and distribute
    that work under terms of your choice, so long as this FAUST
    architecture section is not modified.


 ************************************************************************
 ************************************************************************/
 
#include "netjackaudio_master.h"
#include <assert.h>
 
netjackaudio_master::netjackaudio_master(llvm_dsp* dsp, int buffer_size, int sample_rate):fDSP(dsp)
{
    fDSP = dsp;
    jack_master_t request = { fDSP->getNumInputs() + 1, fDSP->getNumOutputs() + 1, -1, -1, buffer_size, sample_rate, "faustgen_master", -1 };
    jack_slave_t result;
    fNetJack = jack_net_master_open(DEFAULT_MULTICAST_IP, DEFAULT_PORT, "net_master", &request, &result); 
    if (fNetJack) {
        if (sizeof(FAUSTFLOAT) == sizeof(double)) {
            fInputs = new float*[fDSP->getNumInputs() + 1];
            for (int i = 0; i < fDSP->getNumInputs() + 1; i++) {
                fInputs[i] = new float[buffer_size];
            }
            fOutputs = new float*[fDSP->getNumOutputs() + 1];
            for (int i = 0; i < fDSP->getNumOutputs() + 1; i++) {
                fOutputs[i] = new float[buffer_size];
            }
        }
    }
}
        
netjackaudio_master::~netjackaudio_master() 
{ 
   if (fNetJack) {
        jack_net_master_close(fNetJack); 
        fNetJack = 0;
        
        if (sizeof(FAUSTFLOAT) == sizeof(double)) {
            for (int i = 0; i < fDSP->getNumInputs() + 1; i++) {
                delete [] fInputs[i];
            }
            delete [] fInputs;
            
            for (int i = 0; i < fDSP->getNumOutputs() + 1; i++) {
                delete [] fOutputs[i];
            }
            delete [] fOutputs;
        }
    }

    delete fDSP; 
}
        
void netjackaudio_master::buildUserInterface(UI* ui) 
{
    fDSP->buildUserInterface(ui);
}

void netjackaudio_master::remote_compute(int count, float** input, float** output)
{
    int res;
    if ((res = jack_net_master_send(fNetJack, fDSP->getNumInputs(), input, 0, NULL)) < 0) {
        printf("jack_net_master_send failure %d\n", res);
    }  
    
    if ((res = jack_net_master_recv(fNetJack, fDSP->getNumOutputs(), output, 0, NULL)) < 0) {
        printf("jack_net_master_recv failure %d\n", res);
    }
}
    
void netjackaudio_master::double2float_input(int count, double** input)
{
    // Encode input audio data in float
    for (int i = 0; i < fDSP->getNumInputs(); i++) {
        for (int j = 0; j < count; j++) {
            fInputs[i][j] = float(input[i][j]);
        }
    }
}

void netjackaudio_master::float2double_output(int count, double** output)
{
    // Decode output audio data from float
    for (int i = 0; i < fDSP->getNumOutputs(); i++) {
        for (int j = 0; j < count; j++) {
            output[i][j] = double(fOutputs[i][j]);
        }
    }
}

void netjackaudio_master::compute_double(int count, double** input, double** output)
{
    // In last audio port.
    encode_control(fInputs[fDSP->getNumInputs()], count);
    double2float_input(count, input);
    remote_compute(count, fInputs, fOutputs);
    float2double_output(count, output);
    decode_control(fOutputs[fDSP->getNumInputs()], count);
} 

void netjackaudio_master::compute_float(int count, float** input, float** output)
{
    // In last audio port.
    encode_control(fInputs[fDSP->getNumInputs()], count);
    remote_compute(count, input, output);
    decode_control(fOutputs[fDSP->getNumInputs()], count);
}        

void netjackaudio_master::compute(int count, FAUSTFLOAT** input, FAUSTFLOAT** output)
{
    if (sizeof(FAUSTFLOAT) == sizeof(float)) {
        compute_float(count, input, output);
    } else {
        compute_double(count, (double**)input, (double**)output);
    }
}        