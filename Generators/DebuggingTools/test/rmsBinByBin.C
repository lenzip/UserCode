#include <map>
#include <vector>
#include <iostream>
#include <fstream>
#include <string>


#include "TString.h"
#include "TH1.h"
#include "TH2F.h"
#include "TFile.h"
#include "TKey.h"
#include "TClass.h"
#include "TProfile.h"
#include "TCanvas.h"
//#include "TIter.h"

std::vector<TString> getAllHistograms(const TString & fileName1, const char* dir)
{
  TFile file1(fileName1);
  file1.cd(dir);
  TDirectory * d = gDirectory;
  
  std::vector<TString> vnames;
  
  TH1* histo1;
  TKey *key;
  TIter nextkey(d->GetListOfKeys());

  while( key = (TKey*)nextkey() ) {
    TKey * obj = (TKey*)(key->ReadObj());
    if( (obj->IsA()->InheritsFrom("TH1F")) || (obj->IsA()->InheritsFrom("TH1D")) ) {
      histo1 = (TH1*)obj; 
      std::cout << histo1->GetName() << std::endl;
      vnames.push_back(histo1->GetName());
    }
  }

  return vnames;
}

using namespace std;

void plot(TString infile, TString outfile, TString dir){
  TFile out(outfile, "RECREATE");
  ifstream myfile(infile.Data());
  std::string line;
  bool isfirst = true;
  std::vector<TString> names;
  std::map<TString, TProfile*> mhspread; 
  if (myfile.is_open())
  {
    while ( myfile.good() )
    {
      getline(myfile,line);
      //std::cout << line << std::endl;
      if (line == "") break;
      TFile file(line.c_str());
      if (isfirst){
        names = getAllHistograms(line, dir);
        for (int i = 0; i < names.size(); ++i){ 
          TString name = dir+"/"+names[i];
          TH1* h ;
          file.GetObject(name, h);
          double maxy = h->GetBinContent(h->GetMaximumBin());
          TString hspreadname = names[i]+"_spread";
          double minspread = 0.; //max(0.,maxy-0.5*maxy);
          double maxspread = maxy+0.5*maxy;
          out.cd();
          TProfile* h2 = new TProfile(hspreadname, hspreadname, h->GetNbinsX(), h->GetXaxis()->GetXmin(), h->GetXaxis()->GetXmax(), "s");
          mhspread[hspreadname] = h2;                                              
        }  
        isfirst = false;
      }

      for (unsigned int i = 0; i < names.size(); ++i){
        TString name = dir+"/"+names[i];
        TH1* h;
        file.GetObject(name, h);
        TString hspreadname = names[i]+"_spread";
        //cout << hspreadname << endl;
        for (unsigned int bin = 1; bin <= h->GetNbinsX(); ++bin){
          //cout << "bin " << bin << ": " << h->GetBinContent(bin) << endl; 
          mhspread[hspreadname]->Fill(h->GetBinCenter(bin), h->GetBinContent(bin));        
        }
      }
      
    }
    myfile.close();
  }
  for (unsigned int i = 0; i < names.size(); ++i){
    TString hspreadname = names[i]+"_spread";
    TString canvasname = names[i];
    TProfile* profx = mhspread[hspreadname];//->ProfileX("_pfx", 1, -1, "s");
    TString measRMSname = hspreadname+"_RMS";
    TString expRMSname = hspreadname+"_Poisson";
    TString scaledRMSname = hspreadname+"_Scaled";
    out.cd();
    TCanvas* c = new TCanvas(canvasname, canvasname);
    TH1F* measRMS = new TH1F(measRMSname, measRMSname, profx->GetNbinsX(), profx->GetXaxis()->GetXmin(), profx->GetXaxis()->GetXmax());
    TH1F* expRMS  = new TH1F(expRMSname, expRMSname, profx->GetNbinsX(), profx->GetXaxis()->GetXmin(), profx->GetXaxis()->GetXmax());
    TH1F* scaRMS  = new TH1F(scaledRMSname, scaledRMSname, profx->GetNbinsX(), profx->GetXaxis()->GetXmin(), profx->GetXaxis()->GetXmax());
    double factor = 6.465174;
    double ngran = 7987.012987;
    double syst = factor/sqrt(ngran); 
    for (unsigned int bin = 1; bin <= profx->GetNbinsX(); ++bin){
      measRMS->SetBinContent(bin, profx->GetBinError(bin));
      expRMS->SetBinContent(bin, sqrt(profx->GetBinContent(bin)));
      double scalederror = profx->GetBinContent(bin) * sqrt(1/profx->GetBinContent(bin) + syst*syst);
      scaRMS->SetBinContent(bin, scalederror);  
    }
    double max1 = measRMS->GetBinContent(measRMS->GetMaximumBin());
    double max2 = expRMS->GetBinContent(expRMS->GetMaximumBin());
    double max = std::max(max1,max2);
    //double max3 = expRMS->GetBinContent(scaRMS->GetMaximumBin());
    //double max = std::max(max12,max3)+0.10*std::max(max12,max3);
    measRMS->SetLineColor(kRed);
    measRMS->SetLineWidth(2);
    expRMS->SetLineWidth(2);
    scaRMS->SetLineWidth(2);
    scaRMS->SetLineColor(kBlue);
    c->cd();
    measRMS->SetMaximum(max);
    expRMS->SetMaximum(max);
    scaRMS->SetMaximum(max);
    expRMS->Draw();
    measRMS->Draw("sames");
    //scaRMS->Draw("sames");
    string basename(outfile.Data());
    basename.erase(basename.end()-5, basename.end());
    TString sbasename(basename.c_str()); 
    if (i<names.size()-1)
      c->SaveAs(sbasename+".pdf(");
    else c->SaveAs(sbasename+".pdf)");  
  }

  out.Write();
}
