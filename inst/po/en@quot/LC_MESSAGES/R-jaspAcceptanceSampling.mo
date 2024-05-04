��    O      �  k         �  $   �  A   �        $   @  
   e  _   p  !   �  $   �          #     ;     T     k  @   �  W   �  '   	     E	     V	     l	     �	     �	  C   �	  1   �	  1   *
  1   \
     �
     �
     �
  J   �
  <     S   R  �   �  �   r            G   ;  b   �  e   �  i   L     �  Y   �     0  4   G  $   |  	   �     �  y   �  z   ?     �     �     �     �  ?     c   K  E   �  V   �     L     _     f     r     ~     �     �  7   �  ~   �  3   s     �  y   �  n   6     �     �  C   �  s     {   �  g   �     g  a   m     �  $  �  $   �  A        a  $   �  
   �  _   �  !     $   3     X     d     |     �     �  @   �  W     '   ^     �     �     �     �     �  C   �  1   9  1   k  1   �     �     �     �  J     <   V  S   �  �   �  �   �     F     ^  G   |  b   �  e   '  i   �     �  Y         q   4   �   $   �   	   �      �   y   !  z   �!     �!     "     *"     6"  ?   L"  c   �"  E   �"  V   6#     �#     �#     �#     �#     �#     �#     �#  7   �#  ~   5$  3   �$     �$  y   �$  n   w%     �%     &  C   &  s   P&  {   �&  g   @'     �'  a   �'     (        4   /                       7       K   <         ;   O   A   #   N   8   	       J   >      -       
             2   +           B   "          E          !         5       $       1   )       L   F       G   ?                 C         *   :   &   (   9   D      3                  .                        %   @          '   I   M            =                    6       ,   H      0       (Incoming) Proportion Non-conforming 1 -  (Producer's risk) has to be greater than  (consumer's risk). <u>Decision:</u> <b>%s</b> lot. AOQ (Average Outgoing Quality) Curve AOQL: %.3f AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value. ASN (Average Sample Number) Curve ATI (Average Total Inspection) Curve Acc. Number Accept or Reject Lot %s Acceptance Probabilities Acceptance Probability Acceptance Sampling Plan Acceptance number (c) cannot be larger than the sample size (n). Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence. Acceptance probabilities at AQL and RQL Actual P(accept) Analyze Variable Plan Average Outgoing Quality Average Sample Number Average Total Inspection Can not accept or reject lot: sample size has to be greater than 1. Can not calculate AOQ. Check the plan parameters. Can not calculate ASN. Check the plan parameters. Can not calculate ATI. Check the plan parameters. Create Variable Plan Critical Distance (k) Cum. Sample Size Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N). Current plan <b>CAN %s</b> meet the specified risk point(s). Final rejection number (r) needs to be 1 more than the final acceptance number (c). For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).

                                   Else, subsequent stages become redundant. For hypergeometric distribution, N * proportion non-conforming should all be integer values.

Check the values of N and proportion non-conforming. Generated Sampling Plan Historical Standard Deviation If historical standard deviation is unknown, sample size has to be > 1. If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise. Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan. Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan. Lower Specification Limit (LSL) Lower limit for proportion non-conforming items needs to be smaller than the upper limit. Multiple Sampling Plan No valid values found in the plan. Check the inputs. OC (Operating Characteristics) Curve P(accept) Probability of Acceptance Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL. Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL. Prop. Non-conforming Proportion Non-conforming Rej. Number Rejection Probability Rejection number (r) cannot be larger than the sample size (n). Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c). Rejection number (r) has to be larger than the acceptance number (c). Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence. Required P(accept) Sample Sample Mean Sample Size Sample Standard Deviation Sample mean is invalid. Sample size Sample size (n) cannot be larger than the lot size (N). Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>. Sample standard deviation has to be greater than 0. Single Sampling Plan Step size for proportion non-conforming items needs to be smaller than the difference between the upper and lower limits. Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical. Upper Specification Limit (USL) Value Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>) Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints. Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints. Variable plan generated for the current quality constraints is invalid. Modify the quality constraints. Z.LSL Z.LSL = (mean - LSL) / historical standard deviation

Accept lot if Z.LSL >= k, otherwise reject. Z.USL Project-Id-Version: jaspAcceptanceSampling 0.19.0
PO-Revision-Date: 2024-05-04 04:40
Last-Translator: Automatically generated
Language-Team: none
Language: en
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=2; plural=(n != 1);
 (Incoming) Proportion Non-conforming 1 -  (Producer's risk) has to be greater than  (consumer's risk). <u>Decision:</u> <b>%s</b> lot. AOQ (Average Outgoing Quality) Curve AOQL: %.3f AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value. ASN (Average Sample Number) Curve ATI (Average Total Inspection) Curve Acc. Number Accept or Reject Lot %s Acceptance Probabilities Acceptance Probability Acceptance Sampling Plan Acceptance number (c) cannot be larger than the sample size (n). Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence. Acceptance probabilities at AQL and RQL Actual P(accept) Analyze Variable Plan Average Outgoing Quality Average Sample Number Average Total Inspection Can not accept or reject lot: sample size has to be greater than 1. Can not calculate AOQ. Check the plan parameters. Can not calculate ASN. Check the plan parameters. Can not calculate ATI. Check the plan parameters. Create Variable Plan Critical Distance (k) Cum. Sample Size Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N). Current plan <b>CAN %s</b> meet the specified risk point(s). Final rejection number (r) needs to be 1 more than the final acceptance number (c). For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).

                                   Else, subsequent stages become redundant. For hypergeometric distribution, N * proportion non-conforming should all be integer values.

Check the values of N and proportion non-conforming. Generated Sampling Plan Historical Standard Deviation If historical standard deviation is unknown, sample size has to be > 1. If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise. Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan. Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan. Lower Specification Limit (LSL) Lower limit for proportion non-conforming items needs to be smaller than the upper limit. Multiple Sampling Plan No valid values found in the plan. Check the inputs. OC (Operating Characteristics) Curve P(accept) Probability of Acceptance Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL. Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL. Prop. Non-conforming Proportion Non-conforming Rej. Number Rejection Probability Rejection number (r) cannot be larger than the sample size (n). Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c). Rejection number (r) has to be larger than the acceptance number (c). Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence. Required P(accept) Sample Sample Mean Sample Size Sample Standard Deviation Sample mean is invalid. Sample size Sample size (n) cannot be larger than the lot size (N). Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>. Sample standard deviation has to be greater than 0. Single Sampling Plan Step size for proportion non-conforming items needs to be smaller than the difference between the upper and lower limits. Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical. Upper Specification Limit (USL) Value Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>) Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints. Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints. Variable plan generated for the current quality constraints is invalid. Modify the quality constraints. Z.LSL Z.LSL = (mean - LSL) / historical standard deviation

Accept lot if Z.LSL >= k, otherwise reject. Z.USL 