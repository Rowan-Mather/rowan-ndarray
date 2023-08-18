
{
  fetchFromGitHub
}

self: super:

{

    numskull = dontCheck (doJailbreak (haskellPackageGen { }
      (myrtlepkgs.nixpkgs.srcOnly {
        name = "numskull";
        src = builtins.fetchGit {
          url = "ssh://git@github.com/MyrtleSoftware/rowan-ndarray";
          ref = "broadcasting";
          rev = "289909f49742c7f584f89542d22f71cd785dc7a7";
        };
    })));

}
