import React, { useState } from 'react';
import { 
  Box, 
  Button, 
  Heading, 
  Input, 
  Stack, 
  Text, 
  Image, 
  Code, 
  Flex 
} from "@chakra-ui/react";
import { TwoFactorService } from "@/client";
import useCustomToast from "../hooks/useCustomToast";
import { useNavigate } from '@tanstack/react-router';
import { activateSession } from '@/hooks/useAuth';

interface TwoFactorSetupProps {
  onComplete?: () => void;
  tempToken: string;
}



export const TwoFactorSetup: React.FC<TwoFactorSetupProps> = ({ onComplete, tempToken }) => {
  const [step, setStep] = useState<'init' | 'verify'>('init');
  const [qrCode, setQrCode] = useState<string>('');
  const [secret, setSecret] = useState<string>('');
  const [loading, setLoading] = useState<boolean>(false);
  const [password, setPassword] = useState<string>('');
  const [code, setCode] = useState<string>('');
  const [error, setError] = useState<string>('');
  const showToast = useCustomToast();
  const navigate = useNavigate();


  const handleNoTwoFA = async () => {
    try{
      await TwoFactorService.reject2Fa({
        requestBody: {
          temp_token: tempToken
        },
      });
      showToast(
        "Success", 
        "2FA rejected successfully!", 
        "success"
      );
      
      activateSession()
      navigate({ to: "/transactions" });
    } catch (error) {
      console.error('Failed to reject 2FA:', error);
      setError('Failed to reject 2FA');
      showToast(
        "Error", 
        "Failed to reject 2FA.", 
        "error"
      );
    }
  }


  const handleEnable = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');
    setLoading(true);
    
    try {
      const response = await TwoFactorService.enable2Fa({
        requestBody: {
          temp_token: tempToken
        },
      });
      
      setQrCode(response.qr_code);
      setSecret(response.secret);
      setStep('verify');
      showToast(
        "Success", 
        "Scan the QR code with your authenticator app", 
        "success"
      );

    } catch  {
      setError('Failed to enable 2FA');
      showToast(
        "Error", 
        "Failed to enable 2FA.", 
        "error"
      );
    } finally {
      setLoading(false);
    }
  };


  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');
    setLoading(true);
    
    try {
      await TwoFactorService.verify2Fa({
        requestBody: {
          code,
          temp_token: tempToken
        },
      });
      
      showToast(
        "Success", 
        "Two-factor authentication enabled successfully!", 
        "success"
      );
      if (onComplete) {
        onComplete();
      }
    } catch {
      console.error('Failed to verify 2FA code:');
      setError('Invalid verification code');
      showToast(
        "Error", 
        "Invalid verification code. Please try again.", 
        "error"
      );
    } finally {
      setLoading(false);
      sessionStorage.setItem("session_active", "true");
      navigate({ to: "/transactions" });
    }
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="lg" shadow="md">
      <Box>
        {step === 'init' ? (
          <>
            <Heading as="h3" size="md" mb={4}>Enable Two-Factor Authentication</Heading>
            <Text mb={6}>
              Two-factor authentication adds an extra layer of security to your account.
              After enabling, you'll need to enter a verification code from your authenticator app when logging in.
            </Text>
            
            <Box
              as="form"
              onSubmit={handleEnable}
            >
              <Flex gap={2}>
              <Button 
                type="submit" 
                mt={4}
                variant="solid"
                loading={loading}
                disabled={loading}
              >
                Enable 2FA
              </Button>
              <Button 
                variant="outline"
                color="red.500"
                mt={4}
                onClick={handleNoTwoFA}
                disabled={loading}
              >
                Don't Enable 2FA
              </Button>
              </Flex>
            </Box>
          </>
        ) : (
          <>
            <Heading as="h3" size="md" mb={4}>Scan QR Code</Heading>
            <Text mb={6}>
              Scan this QR code with your authenticator app (like Google Authenticator, Authy, or Microsoft Authenticator).
              After scanning, enter the verification code shown in your app.
            </Text>
            
            <Stack gap={6}>
              <Flex justifyContent="center">
                {qrCode && <Image src={qrCode} alt="QR Code" maxWidth="200px" />}
              </Flex>
              
              <Box>
                <Text fontWeight="bold" mb={2}>
                  If you can't scan the QR code, enter this code manually in your authenticator app:
                </Text>
                <Code p={2}>{secret}</Code>
              </Box>
              
              <Box
                as="form"
                onSubmit={handleVerify}
              >
                <Box mb={4}>
                  <Text fontWeight="medium" mb={1}>
                    Verification Code
                  </Text>
                  <Input 
                    id="code"
                    placeholder="Enter 6-digit code"
                    maxLength={6}
                    value={code}
                    onChange={(e) => setCode(e.target.value)}
                    required
                    mt={1}
                  />
                  {error && (
                    <Text color="red.500" fontSize="sm" mt={1}>{error}</Text>
                  )}
                </Box>
                
                <Button 
                  type="submit" 
                  colorScheme="blue" 
                  mt={4}
                  loading={loading}
                  disabled={loading}
                >
                  Verify and Enable
                </Button>
              </Box>
            </Stack>
          </>
        )}
      </Box>
    </Box>
  );
};

export default TwoFactorSetup;
